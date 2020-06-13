;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package flycheck
  :ensure t
  :bind (("C-c e p" . flycheck-previous-error)
         ("C-c e n" . flycheck-next-error))
  :config

  (add-hook 'after-init-hook #'global-flycheck-mode)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist))))

(provide 'config)
;;; config.el ends here
