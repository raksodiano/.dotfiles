;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 5))

(use-package highlight-indentation
  :ensure t
  :commands
  (highlight-indentation-mode
   highlight-indentation-current-column-mode)
  :config (add-hook 'prog-mode-hook 'highlight-indentation-mode))

(use-package highlight-indent-guides
  :init
  (setf highlight-indent-guides-method 'fill)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode))

(use-package fill-column-indicator
  :ensure t
  :bind ([f3] . fci-mode)
  :init (setq fci-rule-width 5
              fci-rule-column 80)
  :config
  (setq fill-column 80)
  (fci-mode 1))

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode))

(use-package nlinum
  :ensure t
  :config
  ;; (setq nlinum-highlight-current-line t)
  (add-hook 'prog-mode-hook 'nlinum-mode))

(use-package smartparens
  :ensure t
  :commands
  (smartparens-mode
   smartparens-strict-mode)
  :bind (("C-}" . sp-forward-slurp-sexp)
         ("M-s" . sp-backward-unwrap-sexp)
         ("C-c [" . sp-select-next-thing)
         ("C-c ]" . sp-select-next-thing-exchange)
         (:map smartparens-strict-mode-map))
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(provide 'config)
;;; config.el ends here
