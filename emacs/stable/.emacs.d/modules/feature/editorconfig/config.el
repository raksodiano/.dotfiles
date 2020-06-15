;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)
(bind-key "RET" 'newline-and-indent)
(bind-key "C-c D" 'duplicate-current-line-or-region) ;; Duplicar linea
(bind-key [f4] 'nlinum-mode)

;; Tamaño de fuente
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(use-package bug-hunter)
;; :ensure t)

(use-package free-keys
  :straight t
  :bind ("C-h C-k" . free-keys))

(use-package multiple-cursors
  :straight t
  :init
  (setq mc/list-file (concat cache-dir ".mc-lists.el"))
  :config
  (bind-keys :prefix-map rakso-mc-map
             :prefix "C-:"
             ("n"   . mc/mark-next-like-this)
             ("N"   . mc/unmark-next-like-this)
             ("M-n" . mc/skip-to-next-like-this)
             ("C-n" . mc/mark-next-lines)
             ("p"   . mc/mark-previous-like-this)
             ("P"   . mc/unmark-previous-like-this)
             ("M-p" . mc/skip-to-previous-like-this)
             ("C-p" . mc/mark-previous-lines)))

(use-package comment-dwim-2
  :straight t
  :bind ("M-;" . comment-dwim-2))

(use-package paredit)
;; :ensure t)

(use-package format-all
  :straight t
  :defer t
  :bind (:map prog-mode-map
              ("<M-f8>" . format-all-buffer)))

(use-package editorconfig
  :straight t
  :diminish ""
  :config
  (editorconfig-mode 1))

(use-package paren
  :init
  (show-paren-mode)
  :config
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-foreground 'show-paren-match "#def")
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(use-package esup
  ;; :ensure t
  :commands esup)

(use-package keyfreq
  :if (daemonp)
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package move-text
  :config (move-text-default-bindings))

(use-package subword
  ;; :ensure nil
  :diminish subword-mode)

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package lorem-ipsum
  :straight t
  :config
  (bind-keys :prefix-map rakso-lorem-ipsum
             :prefix "C-c l"
             ("s" . lorem-ipsum-insert-sentences)
             ("p" . lorem-ipsum-insert-paragraphs)
             ("l" . lorem-ipsum-insert-list))

  (lorem-ipsum-use-default-bindings))

;; (use-package eldoc
;;   :ensure nil
;;   :diminish eldoc-mode
;;   :init
;;   (setf eldoc-idle-delay 1.0))

(use-package ranger
  :init
  (setf ranger-cleanup-eagerly t))

(provide 'config)
;;; config.el ends here
