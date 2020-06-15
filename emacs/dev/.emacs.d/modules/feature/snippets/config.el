;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf yasnippet
      :straight t
      ;; :defer 2
      :diminish yas-minor-mode
      :config
      (yas-global-mode)
      (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(leaf yasnippet-snippets)
;; :ensure t)

(provide 'config)
;;; config.el ends here
