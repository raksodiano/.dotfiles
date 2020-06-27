;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; Colapse blocks
(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'latex-mode-hook 'hs-minor-mode)
(bind-key "C-c S" 'hs-show-all)
(bind-key "C-c s" 'hs-show-block)
(bind-key "C-c H" 'hs-hide-all)
(bind-key "C-c h" 'hs-hide-block)

(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)
(bind-key "RET" 'newline-and-indent)
(bind-key "C-c D" 'duplicate-current-line-or-region) ;; Duplicar linea
(bind-key [f4] 'nlinum-mode)

;; Tamaño de fuente
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)

(defun rakso/setup-mode ()
  (nlinum-mode 1)
  (set-fill-column 120)
  (column-number-mode 1))

(add-hook 'prog-mode-hook 'rakso/setup-mode)

(use-package anzu
  :straight t)

(use-package bug-hunter)
;; :ensure t)

(use-package s
  :straight t)

(use-package dash
  :straight t)

(use-package origami
  :straight t
  :bind (("C-c o :" . origami-recursively-toggle-node)
         ("C-c o a" . origami-toggle-all-nodes)
         ("C-c o t" . origami-toggle-node)
         ("C-c o o" . origami-show-only-node)
         ("C-c o u" . origami-undo)
         ("C-c o U" . origami-redo)
         ("C-c o C-r" . origami-reset))
  :config
  (setq origami-show-fold-header t)
  (add-to-list 'origami-parser-alist '((python-mode . origami-indent-parser)
                                       (emacs-lisp . origami-indent-parser)))
  :init
  (add-hook 'prog-mode-hook 'origami-mode))

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

;; (use-package highlight-numbers
;;   :straight t
;;   :hook
;;   (foo-mode-hook . 'highlight-numbers-mode))

(use-package ranger
  :init
  (setf ranger-cleanup-eagerly t))

(provide 'config)
;;; config.el ends here
