;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package recentf
  :config
  (setq recentf-save-file (concat cache-dir "recentf")
        recentf-exclude '("/tmp/" "/ssh:" "/sudo:" "/scp:" "/scpx:" "/ssh:")
        recentf-max-saved-items 15
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (add-to-list 'recentf-exclude etc-dir)
  (add-to-list 'recentf-exclude cache-dir)
  (recentf-mode +1))

(provide 'config)
;;; config.el ends here
