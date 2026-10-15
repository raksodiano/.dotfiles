;;; treemacs.el -*- lexical-binding: t; -*-
;;; treemacs.el --- Treemacs project drawer (added for Evil everywhere)

;;; Code:

(use-package treemacs
  :commands treemacs
  :config
  (setq treemacs-width 35
        treemacs-persist-file
        (expand-file-name "treemacs-persist" my/emacs-cache)
        treemacs-last-error-persist-file
        (expand-file-name "treemacs-persist-at-last-error" my/emacs-cache))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (add-hook 'treemacs-mode-hook #'(lambda () (display-line-numbers-mode -1))))

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :config
  (treemacs-projectile-mode +1))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode 1))

;; Bind SPC o p to open/select the Treemacs tree for the current project.
(my-leader
 '(("o p" . treemacs)))

(provide 'my-treemacs)
;;; treemacs.el ends here
