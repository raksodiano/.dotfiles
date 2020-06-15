;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; (use-package afternoon-theme
;;   :ensure t
;;   :config
;;   (load-theme 'afternoon t))

(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t)
  (set-face-foreground 'font-lock-variable-name-face "gray"))

(provide 'config)
;;; config.el ends here
