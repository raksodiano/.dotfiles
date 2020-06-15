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
  :straight t
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<left>" . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

;; (use-package switch-window
;;   :straight t
;;   :bind (("C-x o" . switch-window)
;;          ("C-x 1" . switch-window-then-maximize)
;;          ("C-x 2" . switch-window-then-split-below)
;;          ("C-x 3" . switch-window-then-split-right)
;;          ("C-x 0" . switch-window-then-delete)))

(use-package ace-window
  :straight t
  :init
  (setq aw-scope 'frame) ;; this is the key here
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-set-key (kbd "C-x o") 'ace-window))

(use-package windmove
  :straight t)

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package windresize
  :straight t)

(use-package zygospore
  :straight t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("C-x k" . kill-buffer-and-window)))

(provide 'config)
;;; config.el ends here
