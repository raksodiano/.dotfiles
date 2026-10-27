;;; gptel.el -*- lexical-binding: t; -*-
;;; gptel.el --- LLM chat client (gptel)

;;; Commentary:
;; General-purpose LLM chat (karthink/gptel). The default backend is
;; NVIDIA NIM (cloud, key in ~/.secrets/); the local Ollama backend is
;; optional and only works while the Ollama service is running with the
;; models below already pulled (SPC y e i installs ellama's models). The
;; other cloud backends (OpenRouter, Groq, Gemini, HuggingFace, Cohere)
;; are registered only when their key is in ~/.secrets/, so the menu in
;; gptel-menu grows as credentials are added.
;;
;; CREDENTIALS: never in this repository. Cloud backends read their keys
;; from auth-source, which is centralized in ~/.secrets/ for every
;; package (see early-init.el). Add one line per service, in any file
;; inside that directory (or a .gpg file for GPG encryption):
;;
;;   machine integrate.api.nvidia.com login apikey password nvapi-...
;;   machine openrouter.ai login apikey password sk-or-...
;;   machine api.groq.com login apikey password gsk_...
;;   machine generativelanguage.googleapis.com login apikey password AIza...
;;   machine router.huggingface.co login apikey password hf_...
;;   machine api.cohere.com login apikey password ...
;;   machine api.openai.com login apikey password sk-...
;;   machine api.anthropic.com login apikey password sk-ant-...
;;
;; The host of each backend is the `machine' name above, so gptel's own
;; key lookup (gptel-api-key) finds it without any extra wiring.
;;
;; CACHE: `gptel-cache-file' (the conversation cache, which defaults to
;; user-emacs-directory) is redirected to XDG cache in early-init.el,
;; next to every other runtime file this configuration writes.
;;
;; FILE EDITING: `my/gptel-agent' (SPC y A) opens a gptel chat with
;; read_file/write_file/edit_file/list_files tools.  Writes and edits ask
;; for confirmation; sensitive paths are refused.

;;; Code:

;; `string-trim-right', `string-remove-prefix' and `string-suffix-p'
;; (used below) live in subr-x, which is not always loaded yet when
;; this module runs.
(require 'subr-x)

;; Model discovery. gptel only shows in its menu the models declared in
;; `:models' -- it never asks the server what it has (upstream feature
;; request karthink/gptel#567) -- so the catalogs are fetched from the
;; provider instead: OpenAI-compatible endpoints advertise them at
;; /v1/models, Ollama at /api/tags. Only fetched once, when gptel is
;; first loaded (its `use-package' block is deferred), and bounded to
;; five seconds so an unreachable service cannot hang Emacs.

(defun my/gptel-secret (host)
  "Return the API key stored for HOST in ~/.secrets, via auth-source.
Return nil when there is no matching entry. See early-init.el for how
the directory is registered as `auth-sources'."
  (when-let* ((entry (car (auth-source-search
                           :host host :max 1 :require '(:secret))))
              (secret (plist-get entry :secret)))
    ;; Same trailing-newline cleanup gptel applies before sending.
    (string-trim-right
     (if (functionp secret)
         (encode-coding-string (funcall secret) 'utf-8)
       secret)
     "[\n\r]+")))

(defun my/gptel-auth-headers (host)
  "Return the Authorization header for the key of HOST, or nil.
Used by the catalogs that only answer to an authenticated request."
  (when-let* ((key (my/gptel-secret host)))
    `(("Authorization" . ,(concat "Bearer " key)))))

(defun my/gptel-fetch-models (url &optional headers pred)
  "Return the model ids advertised by URL, as a list of strings.
URL must answer with an OpenAI style {\"data\": [{\"id\": ...}]} or an
Ollama/Gemini style {\"models\": [{\"name\": ...}]} payload. HEADERS
is an optional list of (NAME . VALUE) request headers, for the
catalogs that need the key from ~/.secrets/. PRED, when non-nil, is
called with each entry and keeps it when it returns non-nil (several
catalogs list models that cannot answer a chat completion next to the
ones that can). Return nil on any failure (service down, non-JSON
body) so the caller can fall back to a static list."
  (condition-case nil
      (let* ((url-request-extra-headers headers)
             (buf (url-retrieve-synchronously url t t 5)))
        (when buf
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                ;; Skip the HTTP headers up to the blank separator line.
                (re-search-forward "^[[:space:]]*$" nil t)
                (let* ((json (json-parse-string
                              (buffer-substring-no-properties
                               (point) (point-max))
                              :object-type 'alist :array-type 'list))
                       ;; `json-parse-string' interns object keys as
                       ;; symbols, hence `assq' with quoted symbols.
                       (key (if (assq 'data json) 'data 'models))
                       (field (if (eq key 'data) 'id 'name))
                       (entries (cdr (assq key json))))
                  (delq nil
                        (mapcar
                         (lambda (entry)
                           (when (or (null pred) (funcall pred entry))
                             ;; Gemini advertises "models/gemini-..." and
                             ;; gptel appends the id to "/v1beta/models/",
                             ;; so the prefix has to go. Keep trimming until the
                             ;; prefix no longer matches, in case a provider nests
                             ;; it (e.g. "models/models/gemini-...").
                             (let ((id (cdr (assq field entry))))
                               (and id
                                    (let ((n id))
                                      (while (and (string-prefix-p "models/" n)
                                                 (not (eq n (setq n (substring n 7))))))
                                      n)))))
                         entries))))
            (kill-buffer buf))))
    (error nil)))

(use-package gptel
  :commands (gptel gptel-menu gptel-send gptel-rewrite)
  :init
  ;; Read API keys from auth-source (~/.secrets/, see early-init.el).
  ;; `gptel-api-key' already defaults to `gptel-api-key-from-auth-source',
  ;; but it must be passed explicitly as the backends' :key below: in
  ;; gptel 20261013 the default Authorization header does not fall back
  ;; to it when the backend has no key, so no header is sent at all.
  :config
  ;; NVIDIA NIM backend (OpenAI-compatible). The key is looked up in
  ;; ~/.secrets/ by host (machine integrate.api.nvidia.com); add it
  ;; with SPC o A. Only openai/gpt-oss-20b was verified against this
  ;; account; the other models are in /v1/models but may not be enabled
  ;; for it (nemotron returned 404 in testing).
  (gptel-make-openai "NVIDIA"
    :host "integrate.api.nvidia.com"
    :endpoint "/v1/chat/completions"
    :key 'gptel-api-key
    :stream t
    ;; Live catalog from /v1/models. The static list below is only used
    ;; when NIM is unreachable. Note the catalog is what the provider
    ;; lists, not what this key may use: a model without permission
    ;; answers 404 (nemotron did in testing).
    :models (or (my/gptel-fetch-models
                 "https://integrate.api.nvidia.com/v1/models")
                '("openai/gpt-oss-20b"
                  "nvidia/nemotron-3-super-120b-a12b"
                  "z-ai/glm-5.3-flash"
                  "deepseek-ai/deepseek-v4.1-flash")))
  ;; Local Ollama backend. Selectable in gptel-menu; the catalog comes
  ;; from /api/tags, falling back to the models ellama installs with
  ;; SPC y e i when the service is not running.
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models (or (my/gptel-fetch-models "http://localhost:11434/api/tags")
                '("qwen2.5:3b" "qwen2.5:1.5b" "qwen2.5-coder:1.5b")))
  (setq gptel-backend (gptel-get-backend "NVIDIA")
        gptel-model 'openai/gpt-oss-20b)
  ;; Keep the chat readable and its header buttons visible: full frame
  ;; when narrow, otherwise a right side window at least 15% wider than
  ;; Treemacs and wide enough for the header line (see modules/ai/display.el).
  (setq gptel-display-buffer-action '(my/ai-display-buffer))
  ;; Keep tables in AI responses aligned after insertion.
  (add-hook 'gptel-post-response-functions #'my/ai-post-response-tables)
  ;; The other cloud backends: each one is registered only when its key
  ;; is in ~/.secrets/ (see the Commentary for the `machine' hosts), and
  ;; each pulls its own catalog. The static list is only the fallback
  ;; when that catalog cannot be fetched. `:key' is passed explicitly
  ;; for the same reason as the NVIDIA backend above.
  ;;
  ;; OpenRouter: one key for many providers, OpenAI-compatible.
  (when (my/gptel-secret "openrouter.ai")
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :key 'gptel-api-key
      :stream t
      :models (or (my/gptel-fetch-models
                   "https://openrouter.ai/api/v1/models"
                   nil
                   (lambda (entry)
                     ;; Every model is listed twice, once for the normal
                     ;; endpoint and once for its batch variant.
                     (not (string-suffix-p
                           ":batch"
                           (or (cdr (assq 'id entry)) "")))))
                  '("openai/gpt-6-sol" "google/gemini-4-argon"
                    "anthropic/claude-sonnet-5.5"
                    "deepseek/deepseek-v4.1-flash-1017"))))
  ;; Groq (OpenAI-compatible, fast). Its catalog also carries
  ;; transcription and classifier models, which cannot be talked to:
  ;; only the ones that output text are kept.
  (when (my/gptel-secret "api.groq.com")
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :key 'gptel-api-key
      :stream t
      :models (or (my/gptel-fetch-models
                   "https://api.groq.com/openai/v1/models"
                   (my/gptel-auth-headers "api.groq.com")
                   (lambda (entry)
                     (member "text" (cdr (assq 'output_modalities
                                                entry)))))
                  '("openai/gpt-oss-120b" "qwen/qwen3.8-27b"
                    "allam-2-7b"))))
  ;; Google Gemini: native API, not OpenAI-compatible, so it uses
  ;; gptel's own backend. The catalog is fetched with the key itself
  ;; and only keeps the models that answer generateContent (TTS,
  ;; image and audio models are in the same list).
  (when (my/gptel-secret "generativelanguage.googleapis.com")
    (gptel-make-gemini "Gemini"
      :key 'gptel-api-key
      :stream t
      :models (or (my/gptel-fetch-models
                   (concat "https://generativelanguage.googleapis.com"
                           "/v1beta/models?key="
                           (my/gptel-secret
                            "generativelanguage.googleapis.com"))
                   nil
                   (lambda (entry)
                     (member "generateContent"
                             (cdr (assq 'supportedGenerationMethods
                                        entry)))))
                  '("gemini-flash-latest" "gemini-flash-lite-latest"
                    "gemini-pro-latest"))))
  ;; HuggingFace Inference Providers (OpenAI-compatible router).
  (when (my/gptel-secret "router.huggingface.co")
    (gptel-make-openai "HuggingFace"
      :host "router.huggingface.co"
      :endpoint "/v1/chat/completions"
      :key 'gptel-api-key
      :stream t
      :models (or (my/gptel-fetch-models
                   "https://router.huggingface.co/v1/models"
                   (my/gptel-auth-headers "router.huggingface.co"))
                  '("deepseek-ai/DeepSeek-V4.1-Flash"
                    "Qwen/Qwen3.8-27B" "zai-org/GLM-5.3-Flash"
                    "moonshotai/Kimi-K3"))))
  ;; Cohere's own models through its OpenAI-compatible endpoint; the
  ;; catalog is already restricted to chat with ?types=chat.
  (when (my/gptel-secret "api.cohere.com")
    (gptel-make-openai "Cohere"
      :host "api.cohere.com"
      :endpoint "/compatibility/v1/chat/completions"
      :key 'gptel-api-key
      :stream t
      :models (or (my/gptel-fetch-models
                   "https://api.cohere.com/compatibility/v1/models?types=chat"
                   (my/gptel-auth-headers "api.cohere.com"))
                  '("command-a-03-2025" "command-a-plus-05-2026"
                    "c4ai-aya-expanse-32b"))))
  ;; Adding one more provider is the same two steps: a `machine' line
  ;; in ~/.secrets/ (SPC o A) and a backend here, e.g.
  ;; (gptel-make-anthropic "Claude" :key 'gptel-api-key :stream t)
  )

(my-leader
 '(("y y" . gptel)
   ("y A" . my/gptel-agent)
   ("y m" . gptel-menu)
   ("y u" . gptel-send)
   ("y R" . gptel-rewrite)))

(defun my/gptel-sync-model-access (backend)
  "For the regression test: start a probe for BACKEND and wait for the
first result so the model list can be observed synchronously."
  (let* ((host (gptel-backend-host backend))
         (key (my/gptel-secret host)))
    (gptel-model-access backend :creds key)))

(with-eval-after-load 'gptel
  (defun my/gptel-agent--safe-path (path)
    "Return an absolute PATH, refusing sensitive files."
    (let ((full (expand-file-name (or path "") default-directory)))
      (when (or (string-match-p "\\(^\\|/\\)\\.env\\'" (file-name-nondirectory full))
                (string-match-p "/\\.secrets\\(/\\|$\\)" full)
                (string-match-p "/secrets\\(/\\|$\\)" full))
        (error "Refusing to access sensitive path: %s" full))
      full))

  (defun my/gptel-tool-read-file (path)
    "Read and return the contents of PATH."
    (let ((full (my/gptel-agent--safe-path path)))
      (unless (file-readable-p full)
        (error "Cannot read file: %s" full))
      (with-temp-buffer
        (insert-file-contents full)
        (buffer-string))))

  (defun my/gptel-tool-write-file (path content)
    "Write CONTENT to PATH, creating parent directories."
    (let ((full (my/gptel-agent--safe-path path)))
      (make-directory (file-name-directory full) t)
      (with-temp-buffer
        (insert content)
        (write-region (point-min) (point-max) full nil 'silent))
      (format "Wrote %d bytes to %s" (length (or content "")) full)))

  (defun my/gptel-tool-edit-file (path old-string new-string)
    "Replace the first occurrence of OLD-STRING with NEW-STRING in PATH."
    (let* ((full (my/gptel-agent--safe-path path))
           (content (with-temp-buffer
                      (insert-file-contents full)
                      (buffer-string)))
           (start (string-match (regexp-quote old-string) content)))
      (unless start
        (error "Old string not found in %s" full))
      (let* ((end (+ start (length old-string)))
             (new-content (concat (substring content 0 start)
                                  new-string
                                  (substring content end))))
        (with-temp-buffer
          (insert new-content)
          (write-region (point-min) (point-max) full nil 'silent))
        (format "Replaced %d characters in %s" (length old-string) full))))

  (defun my/gptel-tool-list-files (path)
    "List non-hidden files and directories in PATH."
    (let ((full (my/gptel-agent--safe-path path)))
      (unless (file-directory-p full)
        (error "Not a directory: %s" full))
      (mapconcat #'identity
                 (cl-remove-if (lambda (name) (member name '("." "..")))
                               (directory-files full nil nil t))
                 "\\n")))

  (defvar my/gptel-agent-tools
    (list
     (gptel-make-tool
      :function #'my/gptel-tool-read-file
      :name "read_file"
      :description "Read the contents of a file.  Use this before editing a file."
      :args (list '(:name "path" :type string :description "Path to the file"))
      :confirm nil)
     (gptel-make-tool
      :function #'my/gptel-tool-write-file
      :name "write_file"
      :description "Write CONTENT to PATH, creating the file if needed."
      :args (list '(:name "path" :type string :description "Path to the file")
                  '(:name "content" :type string :description "Full file contents"))
      :confirm t)
     (gptel-make-tool
      :function #'my/gptel-tool-edit-file
      :name "edit_file"
      :description "Replace the first occurrence of OLD_STRING with NEW_STRING in PATH."
      :args (list '(:name "path" :type string :description "Path to the file")
                  '(:name "old_string" :type string :description "Exact text to replace")
                  '(:name "new_string" :type string :description "Replacement text"))
      :confirm t)
     (gptel-make-tool
      :function #'my/gptel-tool-list-files
      :name "list_files"
      :description "List files and directories in PATH."
      :args (list '(:name "path" :type string :description "Directory path"))
      :confirm nil))
    "gptel tools used by the in-Emacs file-editing agent."))

(defcustom my/gptel-agent-backend-name "Gemini"
  "Name of the gptel backend used by `my/gptel-agent'.
Change it to \"NVIDIA\", \"OpenRouter\", \"Groq\", \"Gemini\", etc."
  :type 'string
  :group 'gptel)

(defcustom my/gptel-agent-model 'gemini-3.6-flash
  "Model symbol used by `my/gptel-agent'."
  :type 'symbol
  :group 'gptel)

(defun my/gptel-agent (prompt)
  "Start a gptel chat with file-editing tools for PROMPT.
Tools are registered in the `filesystem' category and ask for
confirmation before writing or editing files."
  (interactive "sAsk gptel agent: ")
  (require 'gptel)
  (let* ((preferred (gptel-get-backend my/gptel-agent-backend-name))
         (backend (or preferred gptel-backend))
         (model (if preferred my/gptel-agent-model gptel-model))
         (buf (gptel "*gptel-agent*")))
    (with-current-buffer buf
      (setq-local gptel-backend backend)
      (setq-local gptel-model model)
      (setq-local gptel-tools my/gptel-agent-tools)
      (setq-local gptel-system-prompt
                  "You are an agent that edits files inside Emacs.  Use the provided tools to read, edit, write and list files.  Read a file before editing it, make minimal changes, and explain what you changed.")
      (setq-local gptel-confirm-tool-calls 'auto))
    (with-current-buffer buf
      (gptel-request prompt))
    (display-buffer buf gptel-display-buffer-action)))

(provide 'my-gptel)
;;; gptel.el ends here
