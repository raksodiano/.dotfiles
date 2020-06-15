;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package yasnippet
  :straight t
  :defer 2
  :diminish yas-minor-mode
  :config
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets)
;; :ensure t)

(provide 'config)
;;; config.el ends here
