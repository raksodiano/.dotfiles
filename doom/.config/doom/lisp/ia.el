;;; ia.el --- AI/ELLAMA configuration  -*- lexical-binding: t; -*-

(use-package! ellama
  :defer t
  :commands ellama
  :bind ("C-c e" . ellama-chat)
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-language "Spanish")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "qwen2.5:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "qwen2.5:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-providers
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
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "qwen2.5:0.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "qwen2.5:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-extraction-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1)
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

(defun +my/install-ollama-model-with-progress (model)
  "Install a single ollama MODEL in background with progress tracking."
  (let* ((buffer-name (format " *ollama-pull-%s*" model))
         (proc (start-process
                "ollama-pull"
                buffer-name
                "sh" "-c" (format "ollama pull %s 2>&1" model))))
    (set-process-filter
     proc
     (lambda (proc output)
       (when (string-match "\\([0-9.]+%\\)" output)
         (message "[%s] %s" model (match-string 1 output)))))
    (set-process-sentinel
     proc
     (lambda (proc _event)
       (alert (format "The download of %s has finished" model)
              :title "Ellama"
              :severity 'normal)))))

(defun +my/install-all-ellama-models ()
  "Install all ellama models in background with progress."
  (interactive)
  (let ((models '("qwen2.5:3b"
                  "qwen2.5:1.5b"
                  "qwen2.5-coder:1.5b"
                  "qwen2.5:0.5b"
                  "gemma2:2b"
                  "phi3:mini"
                  "nomic-embed-text")))
    (message "Starting ellama models installation...")
    (dolist (model models)
      (+my/install-ollama-model-with-progress model))))

(map! :leader
      (:prefix ("y" . "IA")
       :desc "Install all LLM models"     "i" #'+my/install-all-ellama-models
       :desc "Chat with IA"              "c" #'ellama-chat
       :desc "Write with IA"             "w" #'ellama-write
       :desc "Translate"                 "t" #'ellama-translate
       :desc "Summarize"                 "s" #'ellama-summarize
       :desc "Code review"               "r" #'ellama-code-review
       :desc "Code complete"             "C" #'ellama-code-complete
       :desc "Code add"                  "a" #'ellama-code-add
       :desc "Code edit"                 "e" #'ellama-code-edit
       :desc "Proofread"                 "p" #'ellama-proofread
       :desc "Improve grammar"           "g" #'ellama-improve-grammar
       :desc "Define word"               "d" #'ellama-define-word
       :desc "Make list"                 "l" #'ellama-make-list
       :desc "Select provider"           "P" #'ellama-provider-select))

(provide '+my-ia)
;;; ia.el ends here
