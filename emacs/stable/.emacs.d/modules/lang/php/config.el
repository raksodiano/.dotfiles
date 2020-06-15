;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package php-refactor-mode
  :after (php-mode)
  :config
  (add-hook 'php-mode-hook 'php-refactor-mode))

(use-package php-mode
  :straight t)

(use-package company-php
  :straight t
  :config
  (defun cfg:php-mode-hook ()
    (interactive)
    (require 'company-php)
    (company-mode t)
    (add-to-list 'company-backends 'company-ac-php-backend))

  (add-hook 'php-mode-hook 'cfg:php-mode-hook))

(add-hook 'php-mode-hook (lambda () (subword-mode 1)))

(provide 'config)
;;; config.el ends here
