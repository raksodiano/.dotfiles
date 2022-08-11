;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package bookmark
  :config
  (setq bookmark-default-file  (concat bookmark-dir "bookmarks"))
  (setf bookmark-save-flag 1)
  (when (file-exists-p bookmark-default-file)
    (bookmark-load bookmark-default-file t)))

(provide 'config)
;;; config.el ends here
