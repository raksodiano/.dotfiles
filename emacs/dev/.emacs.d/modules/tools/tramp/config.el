;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf sudo-edit
      :straight t)

(leaf tramp
      :straight t
      :config
      (setq tramp-persistency-file-name (concat tramp-dir "tramp")
            tramp-auto-save-directory (concat tramp-dir "tramp-autosave")
            tramp-debug-buffer nil
            tramp-verbose 10
            tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>] *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
      ;; (tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh_config")
      ;;                                        (tramp-parse-sconfig "~/.ssh/config")))
      )

(leaf counsel-tramp
  :straight t
  :bind ("C-c T" . counsel-tramp)
  :config
  (add-hook 'counsel-tramp-pre-command-hook '(lambda () (projectile-mode 0)
                                               (editorconfig-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda () (projectile-mode 1)
                                        (editorconfig-mode 1))))

(provide 'config)
;;; config.el ends here
