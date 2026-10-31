;;; ellama.el -*- lexical-binding: t; -*-
;;; ellama.el --- AI via Ollama/Ellama

;;; Code:

(use-package ellama
  :commands (ellama ellama-chat)
  :bind ("C-c e" . ellama-chat)
  :init
  (require 'llm-ollama nil t)
  (setq ellama-language "Spanish")
  (setq ellama-provider
        (make-llm-ollama
         :chat-model "qwen2.5:3b"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-summarization-provider
        (make-llm-ollama
         :chat-model "qwen2.5:1.5b"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-coding-provider
        (make-llm-ollama
         :chat-model "qwen2.5-coder:1.5b"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-providers
        '(("text" . (make-llm-ollama
                     :chat-model "qwen2.5:3b"
                     :embedding-model "nomic-embed-text"))
          ("code" . (make-llm-ollama
                     :chat-model "qwen2.5-coder:1.5b"
                     :embedding-model "nomic-embed-text"))
          ("small-text" . (make-llm-ollama
                           :chat-model "gemma2:2b"
                           :embedding-model "nomic-embed-text"))
          ("small-code" . (make-llm-ollama
                           :chat-model "phi3:mini"
                           :embedding-model "nomic-embed-text"))))
  (setq ellama-naming-provider
        (make-llm-ollama
         :chat-model "qwen2.5:0.5b"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("stop" . "\n"))))
  (setq ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setq ellama-translation-provider
        (make-llm-ollama
         :chat-model "qwen2.5:1.5b"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-extraction-provider
        (make-llm-ollama
         :chat-model "qwen2.5-coder:1.5b"
         :embedding-model "nomic-embed-text"
         :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setq ellama-chat-display-action-function #'display-buffer-full-frame)
  (setq ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  (ellama-context-header-line-global-mode 1)
  (ellama-session-header-line-global-mode 1))

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c C-c") nil)
            t))

(defun my/install-ollama-model-with-progress (model)
  "Install a single ollama MODEL in background with progress tracking."
  (let* ((buffer-name (format " *ollama-pull-%s*" model))
         (proc (start-process
                "ollama-pull" buffer-name
                "sh" "-c" (format "ollama pull %s 2>&1" model))))
    (set-process-filter
     proc
     (lambda (proc output)
       (when (string-match "\\([0-9.]+%\\)" output)
         (message "[%s] %s" model (match-string 1 output)))))
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (when (fboundp 'alert)
         (alert (format "The download of %s has finished" model)
                :title "Ellama" :severity 'normal))))))

(defun my/install-all-ellama-models ()
  "Install all ellama models in background with progress."
  (interactive)
  (let ((models '("qwen2.5:3b" "qwen2.5:1.5b" "qwen2.5-coder:1.5b"
                  "qwen2.5:0.5b" "gemma2:2b" "phi3:mini" "nomic-embed-text")))
    (message "Starting ellama models installation...")
    (dolist (model models)
      (my/install-ollama-model-with-progress model))))

(defvar my/ellama-leader-map (make-sparse-keymap)
  "Keymap for Ellama under `SPC y e'.")

(with-eval-after-load 'my-keys
  (define-key my-leader-map (kbd "y e") my/ellama-leader-map))

(defun my/ellama-leader (bindings)
  "Bind BINDINGS under `my/ellama-leader-map'."
  (declare (indent defun))
  (dolist (binding bindings)
    (define-key my/ellama-leader-map (kbd (car binding)) (cdr binding))))

(my/ellama-leader
 '(("i" . my/install-all-ellama-models)
   ("c" . ellama-chat)
   ("w" . ellama-write)
   ("t" . ellama-translate)
   ("s" . ellama-summarize)
   ("r" . ellama-code-review)
   ("C" . ellama-code-complete)
   ("a" . ellama-code-add)
   ("e" . ellama-code-edit)
   ("p" . ellama-proofread)
   ("g" . ellama-improve-grammar)
   ("d" . ellama-define-word)
   ("l" . ellama-make-list)
   ("P" . ellama-provider-select)))

(provide 'my-ellama)
;;; ellama.el ends here
