;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf smooth-scrolling
  :straight t
  :config
  (setq smooth-scroll-margin 5))

(leaf highlight-indentation
      :straight t
      :commands
      (highlight-indentation-mode
       highlight-indentation-current-column-mode)
      :config (add-hook 'prog-mode-hook 'highlight-indentation-mode))

(leaf highlight-indent-guides
      :straight t
      :init
      (setf highlight-indent-guides-method 'fill)
      (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(leaf which-key
      :straight t
      :diminish which-key-mode
      :config
      (setq which-key-sort-order 'which-key-key-order-alpha)
      (which-key-mode))

(leaf fill-column-indicator
      :straight t
      :bind ([f3] . fci-mode)
      :init (setq fci-rule-width 5
                  fci-rule-column 80)
      :config
      (setq fill-column 80)
      (fci-mode 1))

(leaf visual-fill-column
      :straight t
      :config
      (add-hook 'visual-fill-column-mode-hook #'visual-line-mode))

(leaf nlinum
      :straight t
      :config
      ;; (setq nlinum-highlight-current-line t)
      (add-hook 'prog-mode-hook 'nlinum-mode))

(leaf smartparens
      :straight t
      :commands
      (smartparens-mode
       smartparens-strict-mode)
      :bind (("C-}" . sp-forward-slurp-sexp)
             ("M-s" . sp-backward-unwrap-sexp)
             ("C-c [" . sp-select-next-thing)
             ("C-c ]" . sp-select-next-thing-exchange)
             (smartparens-strict-mode-map))
      :config
      (require 'smartparens-config))

(leaf rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(provide 'config)
;;; config.el ends here
