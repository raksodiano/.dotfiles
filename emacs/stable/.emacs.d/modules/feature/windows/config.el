;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; Mover Buffers
(bind-key "C-<left>" 'windmove-left)
(bind-key "C-<right>" 'windmove-right)
(bind-key "C-<up>" 'windmove-up)
(bind-key "C-<down>" 'windmove-down)

(use-package buffer-move
  :ensure t
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

;; (use-package switch-window
;;   :ensure t
;;   :bind (("C-x o" . switch-window))
;;   :commands (switch-window
;;              switch-window-then-maximize
;;              switch-window-then-split-below
;;              switch-window-then-split-right
;;              switch-window-then-delete
;;              switch-window-then-swap-buffer)
;;   :init
;;   (setq switch-window-shortcut-style 'qwerty
;;         switch-window-qwerty-shortcuts '("a" "s" "d" "f" "g" "h" "j" "k" "l")))

;; (use-package ace-window
;;   :ensure t
;;   :init
;;   (setq aw-scope 'frame) ;; this is the key here
;;   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
;;   (global-set-key (kbd "C-x o") 'ace-window))

(use-package golden-ratio
  :ensure t
  :init
  (golden-ratio-mode 1)
  :config
  (setq
   ;; golden-ratio-auto-scale t
   golden-ratio-adjust-factor 1
   golden-ratio-wide-adjust-factor 1
   golden-ratio-exclude-modes '("dired-mode"
                                "ediff-mode"
                                "eshell-mode"
                                "neotree-mode"
                                "sr-speedbar-mode")))

(use-package windmove)
;; :ensure nil)

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package windresize
  :ensure t )

(use-package zygospore
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("C-x k" . kill-buffer-and-window)))

(provide 'config)
;;; config.el ends here
