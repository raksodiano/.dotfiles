;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package presentation
  :ensure t
  :config (global-set-key
           (kbd "<f6>") (lambda ()
                          (interactive)
                          (if presentation-mode
                              (presentation-mode 0)
                            (presentation-mode 1))
                          (toggle-frame-fullscreen))))

(provide 'config)
;;; config.el ends here
