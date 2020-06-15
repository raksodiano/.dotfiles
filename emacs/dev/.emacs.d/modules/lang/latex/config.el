;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package latex
  :straight auctex
  :init
  (setf TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-newline-function 'newline-and-indent
        LaTeX-item-indent 2
        LaTeX-indent-level 2)
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode-on))

(use-package auctex
  :straight t
  :defer t)

(bind-key "C-c c" 'TeX-clean)

(load "preview-latex.el" nil t t)

;; (use-package cdlatex
;;   :ensure t)

(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)

(use-package magic-latex-buffer
  :straight t)

(add-hook 'latex-mode-hook 'magic-latex-buffer)

(use-package reftex
  :commands turn-on-reftex
  :init
  (progn
    (setq reftex-plug-into-AUCTeX t))
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'latex-mode-hook 'turn-on-reftex))   ; with Emacs latex mode

(use-package bibtex
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook
              (lambda ()
                (set-fill-column 120)))))

(use-package company-auctex
  :straight t
  :config
  (defun cfg:TeX-mode-hook ()
    (company-auctex-init))
  (add-hook 'LaTeX-mode-hook 'cfg:TeX-mode-hook)
  (add-hook 'TeX-mode-hook 'cfg:TeX-mode-hook))

(use-package latex-preview-pane
  :straight t
  :commands (latex-preview-pane-mode))

(use-package latex-math-preview
  :straight t)

;; (use-package latex-extra
;;   :ensure t)

(provide 'config)
;;; config.el ends here
