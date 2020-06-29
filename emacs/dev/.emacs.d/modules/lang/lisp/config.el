;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf cask
      :require t
      :straight t)

(leaf cask-mode
      :require t
      :straight t)

(leaf flycheck-cask
      :require t
      :straight t
      :after (flycheck-mode))

(leaf slime
      :require t
      :straight t
      :init (slime-setup))

(leaf slime-company
      :require t
      :straight t)

(setq inferior-lisp-program "sbcl")
(setq slime-auto-connect 'ask)

(defun my-slime-setup ()
  (require 'slime)
  (slime-setup))

(defvar my--slime-setup-done nil)

(defun my-slime-setup-once ()
  (unless my--slime-setup-done
    (my-slime-setup)
    (setq my--slime-setup-done t)))

(defadvice lisp-mode (before my-slime-setup-once activate)
  (my-slime-setup-once))

;; The SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

(setq inferior-lisp-program "/usr/bin/sbcl --noinform")
(setq slime-contribs '(slime-fancy))

(add-to-list 'slime-contribs 'slime-company)

(slime-setup)

(provide 'config)
;;; config.el ends here
