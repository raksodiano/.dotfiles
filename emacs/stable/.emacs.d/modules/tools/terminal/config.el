;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; (use-package vterm
;;   :ensure t
;;   :custom
;;   (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
;;   (vterm-always-compile-module t))

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-init-function #'(lambda (dir)
                                   (my/goto-term))))

;; (use-package multi-term
;;   :ensure t
;;   :config
;;   (bind-keys :prefix-map rakso-multi-term
;;              :prefix "C-x m"
;;              ("m" . multi-term)
;;              ("<left>" . multi-term-prev)
;;              ("<right>" . multi-term-next)
;;              ("<backspace>" . term-send-backward-kill-word)
;;              ("<delete>" . term-send-forward-kill-word)
;;              ("C-<left>" . term-send-backward-word)
;;              ("C-<right>" . term-send-forward-word)
;;              ("C-j" . term-line-mode)
;;              ("C-k" . term-char-mode)
;;              ("C-v" . scroll-up)
;;              ("y" . term-paste)
;;              ("C-z" . term-stop-subjob)
;;              ("M-DEL" . term-send-backward-kill-word)
;;              ("M-d" . term-send-forward-kill-word))
;;   (custom-set-faces
;;    '(term ((t (:inherit default :foreground "#ffffff"))))
;;    '(term-color-black ((t (:background "#000000" :foreground "#31363b"))))
;;    '(term-color-blue ((t (:background "#2980b9" :foreground "#0099ff"))))
;;    '(term-color-green ((t (:background "#218058" :foreground "#27ae60"))))
;;    '(term-color-magenta ((t (:background "#8e44ad" :foreground "#af81ff"))))
;;    '(term-color-red ((t (:background "#c0392b" :foreground "#f44f4f"))))
;;    '(term-color-white ((t (:background "#acada1" :foreground "#cfd0c2"))))
;;    '(term-color-yellow ((t (:background "#fdbc4b" :foreground "#fdbc4b")))))

;;   (setq multi-term-program "/bin/zsh"))

(provide 'config)
;;; config.el ends here
