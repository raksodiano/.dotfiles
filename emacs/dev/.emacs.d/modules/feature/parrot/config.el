;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(defun my/parrot-animate-when-compile-success (buffer result)
  (if (string-match "^finished" result)
      (parrot-start-animation)))

(leaf parrot
      :straight t
      :config
      (parrot-mode)
      (add-hook 'before-save-hook 'parrot-start-animation)
      (add-to-list 'compilation-finish-functions 'my/parrot-animate-when-compile-success))

(provide 'config)
;;; config.el ends here
