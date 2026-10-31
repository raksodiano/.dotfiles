;;; aidermacs.el -*- lexical-binding: t; -*-
;;; aidermacs.el --- Project-wide AI editing with Aider (aidermacs)

;;; Commentary:
;; Aidermacs drives Aider from inside Emacs: the agent indexes the project
;; with a repo map and edits several files in one go, which is what the
;; other AI modules here cannot do.
;;   modules/ai/gptel.el       chat, plus a small file-editing tool set
;;   modules/ai/agent-shell.el ACP agents (Claude Code, Codex, ...)
;;   modules/ai/aidermacs.el   Aider: repo map, multi-file edits, git
;;
;; Architect mode is the default: one model plans the change and the editor
;; model applies it, the same plan/apply split agentic IDEs use. 1/2/3/4 in
;; the transient menu switch to code, ask, architect and help at any time,
;; and diffs are reviewed in Ediff before being kept.
;;
;; REQUIREMENTS: the `aider' executable, which Emacs does not install:
;;   pipx install aider-chat
;; `aidermacs-program' defaults to the fallback list ("aider-ce" "aider")
;; and takes the first one on `exec-path'; when neither is there the
;; package says so instead of failing silently.
;;
;; CHECKS: `just verify-aidermacs' (offline: load order, SPC y a, the
;; settings, the key export and the history redirection) and
;; `just verify-aidermacs-live' (the same plus a real Aider run on a
;; throwaway project); both are scripts/verify-aidermacs.el.
;;
;; CREDENTIALS: never in this repository. `my/aidermacs-setup-environment'
;; reads the provider keys from ~/.secrets/ through auth-source (see
;; early-init.el) and exports them as the environment variables Aider
;; expects. It runs from `aidermacs-before-run-backend-hook', which
;; aidermacs evaluates inside a copy of `process-environment', so the keys
;; reach the Aider process and nothing else. Add the `machine' hosts listed
;; in `my/aidermacs-key-vars' with SPC o A (see modules/tools/secrets.el).
;;
;; DATA: Aider writes .aider.chat.history.md, .aider.input.history and
;; .aider.llm.history into the project root by default, which would dirty
;; this repository (a stow symlink to git) and every other one. The same
;; hook points the three files at XDG cache instead, one directory per
;; project, the way agent-shell keeps its own data out of the tree. The
;; repo map cache (.aider.tags.cache.v3/) has no such option and stays in
;; the project; Aider's own .gitignore handling keeps it out of git status.

;;; Code:

;; `string-remove-prefix' (used below) lives in subr-x, which is not
;; always loaded yet when this module runs.
(require 'subr-x)

(defun my/aidermacs-secret (host)
  "Return the API key stored for HOST in ~/.secrets, via auth-source.
Returns nil when no entry matches. See early-init.el for how the
directory is registered as `auth-sources'."
  (when-let* ((entry (car (auth-source-search :host host :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defconst my/aidermacs-key-vars
  '(("DEEPSEEK_API_KEY"   . "api.deepseek.com")
    ("OPENAI_API_KEY"     . "api.openai.com")
    ("ANTHROPIC_API_KEY"  . "api.anthropic.com")
    ("GEMINI_API_KEY"     . "generativelanguage.googleapis.com")
    ("OPENROUTER_API_KEY" . "openrouter.ai")
    ("GROQ_API_KEY"       . "api.groq.com")
    ("COHERE_API_KEY"     . "api.cohere.com")
    ;; Aider names this provider nvidia_nim/ (litellm); the host is the
    ;; one gptel already uses for NVIDIA NIM.
    ("NVIDIA_NIM_API_KEY" . "integrate.api.nvidia.com"))
  "Environment variables Aider reads, and the ~/.secrets/ host of each key.

Aider uses the variable matching the provider prefix of the model in
use (deepseek/, openai/, anthropic/, gemini/, openrouter/, groq/,
cohere/, nvidia_nim/), so every provider with an entry in ~/.secrets/
shows up as a usable model. One without a key is simply not exported.")

(defun my/aidermacs-data-dir ()
  "Return the directory holding the current project's Aider runtime files.
One directory per project under XDG cache, named after the flattened
project path (e.g. home-user-dotfiles). The hook below runs with
`default-directory' set to the project root, so that is what names it."
  (let ((project (replace-regexp-in-string
                  "/" "-"
                  (string-remove-prefix
                   "/" (directory-file-name default-directory)))))
    (expand-file-name project (expand-file-name "aider" my/emacs-cache))))

(defun my/aidermacs-setup-environment ()
  "Prepare what Aider needs before it starts.

Runs from `aidermacs-before-run-backend-hook' inside a copy of
`process-environment', so everything set here reaches the Aider process
and is discarded when it exits. Two things happen: the provider keys in
~/.secrets/ are exported as the environment variables Aider expects
\(see `my/aidermacs-key-vars'), and the chat, input and LLM history
files are pointed at XDG cache so Aider does not write them into the
project."
  (let ((keys 0))
    (dolist (entry my/aidermacs-key-vars)
      (when-let* ((secret (my/aidermacs-secret (cdr entry))))
        (setenv (car entry) secret)
        (setq keys (1+ keys))))
    (let ((dir (my/aidermacs-data-dir)))
      (make-directory dir t)
      (setenv "AIDER_CHAT_HISTORY_FILE" (expand-file-name "chat.history.md" dir))
      (setenv "AIDER_INPUT_HISTORY_FILE" (expand-file-name "input.history" dir))
      (setenv "AIDER_LLM_HISTORY_FILE" (expand-file-name "llm.history" dir))
      (message "my/aidermacs: %d key(s) from ~/.secrets, history in %s" keys dir))))

(use-package aidermacs
  :commands (aidermacs-transient-menu)
  :init
  ;; NOTE: plain setq in :init, not :custom. aidermacs is deferred here, so
  ;; `:custom' runs while these variables do not exist yet and use-package
  ;; warns void-variable on every start. defcustom keeps a value that is
  ;; already set, so setting them here survives the package loading later.
  ;;
  ;; Architect mode: one model plans, the editor model applies the result.
  (setq aidermacs-default-chat-mode 'architect)
  ;; Aider names models as PROVIDER/MODEL. `o' in the transient menu
  ;; (`aidermacs-change-model') lists what the installed Aider knows,
  ;; local Ollama models included as ollama_chat/qwen2.5-coder:1.5b.
  ;;
  ;; OpenRouter rather than deepseek/ directly: the deepseek provider
  ;; needs a DeepSeek account, and ~/.secrets/ holds an OpenRouter key
  ;; already. Aider routes OpenRouter models as openrouter/VENDOR/MODEL,
  ;; and the OpenRouter catalogue carries gpt-oss-120b, free and paid
  ;; Gemini and Grok builds, etc. -- pick one with `o'.
  (setq aidermacs-default-model "openrouter/deepseek/deepseek-chat")
  ;; No update check on launch: nothing in this configuration reaches the
  ;; network while starting a tool.
  (setq aidermacs-extra-args '("--no-check-update"))
  :config
  (add-hook 'aidermacs-before-run-backend-hook #'my/aidermacs-setup-environment))

(my-leader
 '(("y a" . aidermacs-transient-menu)))

(provide 'my-aidermacs)
;;; aidermacs.el ends here
