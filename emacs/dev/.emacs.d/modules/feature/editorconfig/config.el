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
  ;; (nlinum-mode 1)
  (set-fill-column 120)
  (column-number-mode 1))

(add-hook 'prog-mode-hook 'rakso/setup-mode)

(leaf anzu
  :straight t)

(leaf bug-hunter
      :straight t)
;; :ensure t)

(leaf s
  :straight t)

(leaf dash
  :straight t)

(leaf origami
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

(leaf free-keys
  :straight t
  :bind ("C-h C-k" . free-keys))

(leaf multiple-cursors
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

(leaf comment-dwim-2
  :straight t
  :bind ("M-;" . comment-dwim-2))

(leaf paredit
      :straight t)
;; :ensure t)

(leaf format-all
      :straight t
      :leaf-defer t
      :bind (prog-mode-map
             ("<M-f8>" . format-all-buffer)))

(leaf editorconfig
  :straight t
  :diminish ""
  :config
  (editorconfig-mode 1))

(leaf paren
      :straight t
      :init
      (show-paren-mode)
      :config
      (set-face-background 'show-paren-match (face-background 'default))
      (set-face-foreground 'show-paren-match "#def")
      (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(leaf esup
      :straight t
      ;; :ensure t
      :commands esup)

(leaf keyfreq
      :straight t
      :if (daemonp)
      :config
      (keyfreq-mode 1)
      (keyfreq-autosave-mode 1))

(leaf move-text
      :straight t
      :config (move-text-default-bindings))

(leaf subword
      :straight t
      ;; :ensure nil
      :diminish subword-mode)

(leaf hungry-delete
      :straight t
      :diminish hungry-delete-mode
      :config
      (global-hungry-delete-mode))

(leaf lorem-ipsum
      :straight t
      :config
      (bind-keys :prefix-map rakso-lorem-ipsum
                 :prefix "C-c l"
                 ("s" . lorem-ipsum-insert-sentences)
                 ("p" . lorem-ipsum-insert-paragraphs)
                 ("l" . lorem-ipsum-insert-list))

      (lorem-ipsum-use-default-bindings))

;; (leaf highlight-numbers
;;   :straight t
;;   :hook
;;   (foo-mode-hook . 'highlight-numbers-mode))

(leaf ranger
      :straight t
      :init
      (setf ranger-cleanup-eagerly t))

(provide 'config)
;;; config.el ends here
