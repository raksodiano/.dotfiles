;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf nginx-mode
      :straight t
      :mode
      ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode))

(provide 'config)
;;; config.el ends here
