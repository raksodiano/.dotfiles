;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Herramienta simple para el completado del minibuffer de Emacs.

;;; Code:
(leaf flx
      :straight t)

(leaf avy
      :straight t)

(leaf ivy
      :straight t
      :diminish ivy-mode
      ;; :bind (("C-! r" . swiper)
      ;;        ("C-! s" . swiper)
      ;;        ("C-! a" . swiper-all)
      ;;        (:map ivy-mode-map))
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

(leaf lsp-ivy
      :straight t)

;; (leaf ivy-rich
;;   :init (setf ivy-format-function #'ivy-format-function-line)
;;   :config (ivy-rich-mode 1))

(leaf swiper
      :straight t)
;; :bind
;; (:map read-expression-map
;;       ("C-r" . counsel-expression-history)))

(leaf counsel
      :straight t
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

(leaf imenu-anywhere
  :straight t)

(provide 'config)
;;; config.el ends here
