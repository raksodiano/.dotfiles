;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package projectile
  :straight t
  :diminish projectile-mode
  :config
  (setq projectile-known-projects-file (concat cache-dir "projectile-bookmarks.eld")
        projectile-cache-file (concat cache-dir "projectile.cache")
        projectile-file-exists-remote-cache-expire (* 10 60)
        projectile-indexing-method 'alien
        projectile-enable-caching t
        projectile-completion-system 'ivy)
  (projectile-mode))

(use-package counsel-projectile
  :straight t
  :after (counsel projectile)
  :bind ("C-x r R" . counsel-projectile-rg)
  :config
  (setq counsel-projectile-rg-options-history (list "-uuu"))
  (add-hook 'text-mode-hook 'counsel-projectile-mode)
  (add-hook 'prog-mode-hook 'counsel-projectile-mode))

(use-package term-projectile
  :straight t)

(use-package rg
  :straight t
  :config (setq rg-command-line-flags (list "-uuu")))

(setq projectile-switch-project-action 'neotree-projectile-action)

(provide 'config)
;;; config.el ends here
