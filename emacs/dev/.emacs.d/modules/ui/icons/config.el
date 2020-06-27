;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf all-the-icons
      :straight t
      :commands (all-the-icons-octicon
                 all-the-icons-faicon
                 all-the-icons-fileicon
                 all-the-icons-wicon
                 all-the-icons-material
                 all-the-icons-alltheicon
                 all-the-icons-install-fonts))

(leaf all-the-icons-dired
      :straight t
      :hook
      (dired-mode-hook . all-the-icons-dired-mode))

(leaf all-the-icons-gnus
  :straight t)

(leaf all-the-icons-ivy
  :straight t
  :config
  (all-the-icons-ivy-setup))

(leaf treemacs-icons-dired
  :after treemacs dired
  :straight t
  :config
  (treemacs-icons-dired-mode))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(provide 'config)
;;; config.el ends here
