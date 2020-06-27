;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf logview
      :straight t
      :config (add-hook 'logview-mode-hook 'auto-revert-mode))

(provide 'config)
;;; config.el ends here
