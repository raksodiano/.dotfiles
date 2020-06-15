;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Herramienta simple para el completado del minibuffer de Emacs.

;;; Code:
(use-package flx
  :straight t)

(use-package avy
  :straight t)

(use-package ivy
  :straight t
  :diminish ivy-mode
  :bind (("C-! r" . swiper)
         ("C-! s" . swiper)
         ("C-! a" . swiper-all)
         (:map ivy-mode-map))
  :config
  (ivy-mode 1)
  (setq ivy-wrap t
        ivy-virtual-abbreviate 'full
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))
  (setq ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil))

;; (use-package ivy-rich
;;   :init (setf ivy-format-function #'ivy-format-function-line)
;;   :config (ivy-rich-mode 1))

(use-package swiper
  :bind
  (:map read-expression-map
        ("C-r" . counsel-expression-history)))

(use-package counsel
  :ensure t
  :bind ((("M-x" . counsel-M-x)
          ("M-y" . counsel-yank-pop)
          ("M-SPC" . counsel-shell-history)
          ("C-c b" . counsel-imenu)
          ("C-h f" . counsel-describe-function)
          ("C-h v" . counsel-describe-variable)
          ("C-h b" . counsel-descbinds)
          ("C-x C-f" . counsel-find-file)
          ("C-x r r" . counsel-rg)))
  :config
  (setq counsel-find-file-at-point t))

(use-package imenu-anywhere
  :straight t)

(provide 'config)
;;; config.el ends here
