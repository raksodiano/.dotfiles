;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package all-the-icons
  :straight t
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon
             all-the-icons-install-fonts))

(use-package all-the-icons-dired
  :straight t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-gnus
  :straight t)

(use-package all-the-icons-ivy
  :straight t
  :config
  (all-the-icons-ivy-setup))

(provide 'config)
;;; config.el ends here
