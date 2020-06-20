;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package sudo-edit
  :straight t)

(use-package tramp
  :config
  (setq tramp-persistency-file-name (concat tramp-dir "tramp")
        tramp-auto-save-directory (concat tramp-dir "tramp-autosave")
        tramp-debug-buffer nil
        tramp-verbose 10
        tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>] *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
  (tramp-set-completion-function "ssh" '((tramp-parse-sconfig "/etc/ssh_config")
                                         (tramp-parse-sconfig "~/.ssh/config"))))

(use-package counsel-tramp
  :straight t
  :bind ("C-c T" . counsel-tramp)
  :config
  (add-hook 'counsel-tramp-pre-command-hook '(lambda () (projectile-mode 0)
                                               (editorconfig-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda () (projectile-mode 1)
                                        (editorconfig-mode 1))))

(provide 'config)
;;; config.el ends here
