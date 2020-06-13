;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(defun my/parrot-animate-when-compile-success (buffer result)
  (if (string-match "^finished" result)
      (parrot-start-animation)))

(use-package parrot
  :ensure t
  :config
  (parrot-mode)
  (add-hook 'before-save-hook 'parrot-start-animation)
  (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

(provide 'config)
;;; config.el ends here
