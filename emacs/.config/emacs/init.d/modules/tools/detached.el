;;; detached.el -*- lexical-binding: t; -*-
;;; detached.el --- Background commands (split out)

;;; Code:

(use-package detached
  :commands (detached-shell-command detached-init)
  :hook (after-init . detached-init)
  :custom
  (detached-notification-function
   'detached-state-transition-notifications-message)
  (detached-db-directory "~/.cache/emacs/detached")
  (detached-session-directory (temporary-file-directory))
  (detached-init-block-list '(dired-rsync dired))
  :config
  (global-set-key (kbd "M-&") #'detached-shell-command))

(use-package detached-consult
  :ensure nil
  :defer t
  :after detached
  :config
  (my-leader
   '(("o s" . detached-consult-session))))

(use-package detached-compile
  :ensure nil
  :defer t
  :after detached
  :config
  (my-leader
   '(("c c" . detached-compile)
     ("c C" . detached-compile-recompile))))

(use-package detached-list
  :ensure nil
  :defer t
  :after detached
  :config
  (my-leader
   '(("o S" . detached-list-sessions)))
  (evil-set-initial-state 'detached-list-mode 'emacs))

(provide 'my-detached)
;;; detached.el ends here
