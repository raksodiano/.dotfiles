;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package nginx-mode
  :straight t
  :mode
  ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(provide 'config)
;;; config.el ends here
