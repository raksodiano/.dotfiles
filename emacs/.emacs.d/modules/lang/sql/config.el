;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package edbi-database-url
  :straight t)

(use-package edbi-minor-mode
  :straight t
  :config
  (add-hook 'sql-mode-hook 'edbi-minor-mode))

(use-package company-edbi
  :straight t
  :init
  (defun cfg:edbi-mode-hook()
    (add-to-list 'company-backends 'company-edbi))
  (add-hook 'edbi:sql-mode-hook 'cfg:edbi-mode-hook))

(use-package sqlup-mode
  :straight t
  :bind ("C-c u" . sqlup-capitalize-keywords-in-region)
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  (add-hook 'edbi:sql-mode-hook 'sqlup-mode)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode))

(use-package sql-indent
  :straight t
  :bind (:map sql-mode-map (("C-c \\" . sql-indent-buffer)))
  :config (eval-after-load "sql"
            '(load-library "sql-indent")))

(use-package sqlformat
  :straight t
  :config (add-hook 'sql-mode-hook 'sqlformat-mode))

(provide 'config)
;;; config.el ends here
