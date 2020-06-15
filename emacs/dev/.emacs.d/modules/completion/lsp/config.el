;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; (leaf lsp-mode
;;       :straight t
;;       ;; :commands (lsp lsp-deferred)
;;       :init
;;       (setq lsp-prefer-capf t)
;;       ;; 10Mb LSP consume large payloads so a higher value is required
;;       (setq read-process-output-max (* 10 1024 1024))
;;       ;; :require (lsp-mode lsp-clients)
;;       ;; :config ((lsp-session-file . ,(concat cache-dir "lsp-session"))
;;       ;;          (lsp-auto-guess-root . t)
;;       ;;          (lsp-enable-folding . nil)
;;       ;;          (lsp-enable-snippet . t)
;;       ;;          (lsp-enable-symbol-highlighting . nil)
;;       ;;          (lsp-idle-delay . 0.500)
;;       ;;          (lsp-inhibit-message . t)
;;       ;;          (lsp-message-project-root-warning . t)
;;       ;;          (lsp-prefer-capf . t)
;;       ;;          (lsp-prefer-flymake . t)
;;       ;;          (lsp-print-io . nil)
;;       ;;          (lsp-restart . 'interactive)
;;       ;;          (lsp-signature-auto-activate . nil)
;;       ;;          (lsp-eldoc-render-all . nil))
;;       :hook ((python-mode . lsp)
;;              (lsp-after-open-hook . lsp-enable-imenu))
;;       :commands lsp)

;; ;; ref: https://gitlab.com/shackra/emacs/commit/b0df30fe744e4483a08731e6a9f6482ab408124c
;; (defvar-local conf:lsp-on-change-exist nil
;;   "indica si la función `lsp-on-change' estaba insertada en `after-change-functions'")

;; (defun conf:lsp-on-change-modify-hook ()
;;   "Remueve o agrega `lsp-on-change' de `after-change-functions'"
;;   (if (not conf:lsp-on-change-exist)
;;       ;; quita la función, solamente si estaba insertada desde un principio
;;       (when (memq 'lsp-on-change after-change-functions)
;;         (setq conf:lsp-on-change-exist t)
;;         (remove-hook 'after-change-functions 'lsp-on-change t))
;;     ;; agrega la función
;;     (add-hook 'after-change-functions #'lsp-on-change nil t)
;;     (setq conf:lsp-on-change-exist nil)))

;; (leaf lsp-ui
;;       :straight t
;;       :after lsp-mode
;;       :hook (lsp-mode . lsp-ui-mode)
;;       :custom
;;       (lsp-ui-doc-enable t)
;;       (lsp-eldoc-hook nil)
;;       (lsp-ui-doc-delay 2)
;;       :config
;;       (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
;;       (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
;; ;; :commands lsp-ui-mode
;; ;; :bind (lsp-ui-mode-map
;; ;;        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;; ;;        ([remap xref-find-references] . lsp-ui-peek-find-references))
;; ;; :setq  ((lsp-ui-sideline-enable . t)
;; ;;         (lsp-ui-sideline-ignore-duplicate . t)
;; ;;         (lsp-ui-sideline-show-hover . nil)
;; ;;         (lsp-ui-doc-enable . nil))
;; ;; :config (lsp-ui-mode))

(leaf lsp-mode
      :straight t
      :commands (lsp lsp-deferred)
      :require (lsp-mode lsp-clients)
      :pre-setq `((lsp-session-file . ,(concat cache-dir "lsp-session"))
                  (lsp-auto-guess-root . t)
                  (lsp-enable-folding . nil)
                  (lsp-enable-snippet . t)
                  (lsp-enable-symbol-highlighting . nil)
                  (lsp-idle-delay . 0.500)
                  (lsp-inhibit-message . t)
                  (lsp-message-project-root-warning . t)
                  (lsp-prefer-capf . t)
                  (lsp-prefer-flymake . t)
                  (lsp-print-io . nil)
                  (lsp-restart . 'interactive)
                  (lsp-signature-auto-activate . nil)
                  (lsp-eldoc-render-all . nil))
      :hook (lsp-after-open-hook . lsp-enable-imenu))

;; ref: https://gitlab.com/shackra/emacs/commit/b0df30fe744e4483a08731e6a9f6482ab408124c
(defvar-local conf:lsp-on-change-exist nil
  "indica si la función `lsp-on-change' estaba insertada en `after-change-functions'")

(defun conf:lsp-on-change-modify-hook ()
  "Remueve o agrega `lsp-on-change' de `after-change-functions'"
  (if (not conf:lsp-on-change-exist)
      ;; quita la función, solamente si estaba insertada desde un principio
      (when (memq 'lsp-on-change after-change-functions)
        (setq conf:lsp-on-change-exist t)
        (remove-hook 'after-change-functions 'lsp-on-change t))
    ;; agrega la función
    (add-hook 'after-change-functions #'lsp-on-change nil t)
    (setq conf:lsp-on-change-exist nil)))

(leaf lsp-ui
      :straight t
      :after lsp-mode
      :commands lsp-ui-mode
      :bind (lsp-ui-mode-map
             ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
             ([remap xref-find-references] . lsp-ui-peek-find-references))
      :setq  ((lsp-ui-sideline-enable . t)
              (lsp-ui-sideline-ignore-duplicate . t)
              (lsp-ui-sideline-show-hover . nil)
              (lsp-ui-doc-enable . nil))
      :config (lsp-ui-mode))

(provide 'config)
;;; config.el ends here
