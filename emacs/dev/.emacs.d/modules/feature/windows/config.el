;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; Resize Windows
(global-set-key (kbd "M-S-<up>") 'shrink-window)
(global-set-key (kbd "M-S-<down>") 'enlarge-window)
(global-set-key (kbd "M-S-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-S-<right>") 'shrink-window-horizontally)

;; Mover Buffers
(bind-key "C-<left>" 'windmove-left)
(bind-key "C-<right>" 'windmove-right)
(bind-key "C-<up>" 'windmove-up)
(bind-key "C-<down>" 'windmove-down)

(leaf buffer-move
      :straight t
      :bind (("C-S-<up>" . buf-move-up)
             ("C-S-<down>" . buf-move-down)
             ("C-S-<left>" . buf-move-left)
             ("C-S-<right>" . buf-move-right)))

(leaf ace-window
      :straight t
      :init
      (setq aw-scope 'frame) ;; this is the key here
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
      (global-set-key (kbd "C-x o") 'ace-window))

(leaf winum
      :straight t
      :init
      (setq winum-keymap
            (let ((map (make-sparse-keymap)))
              (define-key map (kbd "C-`") 'winum-select-window-by-number)
              (define-key map (kbd "C-²") 'winum-select-window-by-number)
              (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
              (define-key map (kbd "M-1") 'winum-select-window-1)
              (define-key map (kbd "M-2") 'winum-select-window-2)
              (define-key map (kbd "M-3") 'winum-select-window-3)
              (define-key map (kbd "M-4") 'winum-select-window-4)
              (define-key map (kbd "M-5") 'winum-select-window-5)
              (define-key map (kbd "M-6") 'winum-select-window-6)
              (define-key map (kbd "M-7") 'winum-select-window-7)
              (define-key map (kbd "M-8") 'winum-select-window-8)
              map))
      :config
      (winum-mode))

(leaf windmove
      :straight t)

(leaf winner
      :straight t
      :config
      (winner-mode 1))

(leaf windresize
      :straight t)

(leaf zygospore
  :straight t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("C-x k" . kill-buffer-and-window)))

(provide 'config)
;;; config.el ends here
