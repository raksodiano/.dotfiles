;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf man
      :config
      (progn
        (defun imalison:fontify-man-page-buffer ()
          (interactive)
          (read-only-mode -1)
          (Man-fontify-manpage)
          (read-only-mode +1))))

(provide 'config)
;;; config.el ends here
