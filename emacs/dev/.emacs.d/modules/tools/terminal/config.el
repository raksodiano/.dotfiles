;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf vterm
      :straight t
      ;; :custom
      ;; (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=yes")
      ;; (vterm-always-compile-module t)
      )

(leaf multi-vterm
      :straight t
      :bind (("C-c 4 v" . multi-vterm-dedicated-open)
             ("C-c 5 v" . multi-vterm)
             ("C-c t v" . multi-vterm-dedicated-toggle)
             ("C-c 0 v" . multi-term-dedicated-close)))

(leaf vterm-toggle
      :straight t
      :bind (("C-c t v" . vterm-toggle)
             ;; vterm-mode-map (("C-M-n" . vterm-toggle-forward)
             ;;                 ("C-M-p" . vterm-toggle-backward))
             ))

(leaf eshell-toggle
      :straight t
      ;; :custom
      ;; (eshell-toggle-size-fraction 3)
      ;; (eshell-toggle-use-projectile-root t)
      ;; (eshell-toggle-run-command nil)
      ;; (eshell-toggle-init-function #'eshell-toggle-init-eshell)
      ;; (eshell-toggle-init-function #'(lambda (dir)
      ;;                                  (my/goto-term)))
      :bind ("C-c t e" . eshell-toggle))

;; open up a mini-eshell
(defun quarter-window-vertically ()
  "create a new window a quarter size of the current window"
  (split-window-vertically)
  (other-window 1)
  (split-window-vertically)
  (other-window -1)
  (delete-window))

(defun open-mini-eshell ()
  "open a mini-eshell in a small window at the bottom of the current window"
  (interactive)
  (quarter-window-vertically)
  (other-window 1)
  (eshell))

(global-set-key (kbd "C-. m") 'open-mini-eshell)

;; (leaf multi-term
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
