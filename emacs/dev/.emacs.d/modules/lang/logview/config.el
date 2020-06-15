;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package logview
  :straight t
  :config (add-hook 'logview-mode-hook 'auto-revert-mode))

(provide 'config)
;;; config.el ends here
