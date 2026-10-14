;;; my-editing.el -*- lexical-binding: t; -*-
;;; my-editing.el --- Editing behaviour, snippets and formatting  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's :editor module, the tree-sitter settings in
;; lisp/lsp.el and the LaTeX/Markdown/Web/KDL sections of config.el.
;; Doom's `format +onsave' is reimplemented locally because
;; +format-on-save-enabled-modes was a Doom variable.

;;; Code:

;;; ### Snippets

;; Doom's snippets module.
;; NOTE: these two go in :config, not :custom. use-package quotes the
;; value given to :custom, and a quoted list containing a dotted pair
;; trips the reader in Emacs 31.1. :config runs after the package is
;; loaded, so the defcustom cannot overwrite these values either.
(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-bases
        `((user . ,(expand-file-name "snippets" my/config-dir)))
        yas-key-bindings '(("," . yas-expand-key))))

;; yasnippet-snippets registers its snippets through an autoloaded entry
;; point; the old yas-load-snippets function no longer exists.
(use-package yasnippet-snippets
  :config
  (yasnippet-snippets-initialize))

;;; ### Multiple cursors

(use-package multiple-cursors
  :bind-keymap (("C-c m" . multiple-cursors-map)))

;;; ### Electric pairs and indentation

(use-package smartparens
  ;; NOTE: smartparens-strict-mode, smartparens-undo-backward and
  ;; smart-backspace-deletes-quote were removed from smartparens; the
  ;; only customisable options left are the hooks below.
  :custom
  (smartparens-enabled-hook nil)
  (smartparens-disabled-hook nil)
  :config
  (smartparens-mode 1))

;; paredit is built into Emacs; no package needed.
(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook
                scheme-mode-hook lisp-data-mode-hook))
  (add-hook hook #'paredit-mode))

;; Doom's electric: keyword-aware electric indentation.
(electric-indent-mode 1)

;;; ### Formatting on save

;; Replaces Doom's (format +onsave) and the +format-on-save-enabled-modes
;; list that lived in lisp/lsp.el.

(defvar my/format-on-save-modes
  '(python-mode js-mode typescript-mode c-mode c++-mode
    sh-mode json-mode yaml-mode)
  "Modes whose buffers are formatted before saving.")

(defcustom my/black-args '("-q")
  "Command-line arguments passed to black."
  :type '(repeat string))

(defun my-buffer-formatter ()
  "Return the formatter command for the current buffer, or nil."
  (cond
   ((derived-mode-p 'python-mode)
    (when (executable-find "black") (cons "black" my/black-args)))
   ((derived-mode-p '(js-mode typescript-mode))
    (when (executable-find "prettier") (list "prettier" "--write")))
   ((derived-mode-p 'json-mode)
    (when (executable-find "python") (list "python" "-m" "json.tool")))
   ((derived-mode-p 'sh-mode)
    (when (executable-find "shfmt") (list "shfmt" "-i=2" "-w")))
   (t nil)))

(defun my-format-on-save ()
  "Format the current buffer with its formatter, if one is available."
  (interactive)
  (let ((command (my-buffer-formatter))
        (file (or (buffer-file-name) (buffer-name))))
    (when (and command (file-exists-p file))
      (save-buffer)
      (apply #'call-process command nil nil nil file)
      (revert-buffer :ignore-auto :noconfirm :preserve-modes t)
      (set-buffer-modified-p nil))))

(add-hook 'before-save-hook #'my-format-on-save nil t)

;;; ### Tree-sitter

;; Doom's tree-sitter module plus treesit-auto from packages.el.
(use-package treesit-auto
  :config
  (setq treesit-auto-install t
        treesit-font-lock-level 4))

;; Folding for languages without native support, ported from config.el.
(add-hook 'sh-mode-hook #'outline-minor-mode)
(add-hook 'markdown-mode-hook #'outline-minor-mode)
(setq hs-isearch-open t)

;;; ### Language modes not shipped as Doom modules

;; dotenv-mode, ported from config.el (the regex is fixed: the original
;; pattern had a literal quote that could never match).
(use-package dotenv-mode
  :mode (("\\.env\\.?.*\\'" . dotenv-mode)))

(use-package kdl-mode
  :mode ("\\.kdl\\'" . kdl-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.s?jsx?\\'" . web-mode)))

;; Astro and Svelte are handled by web-mode, ported from config.el.
(dolist (pattern '("\\.astro\\'" "\\.svelte\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'web-mode)))

;; docker-compose files as YAML, ported from config.el.
(add-to-list 'auto-mode-alist
             '("docker-compose[.-]?\\(yml\\|yaml\\)\\'" . yaml-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;;; ### Tree-sitter grammars to install

;; Ported from +my/treesit-grammars-to-install in lisp/lsp.el, trimmed to
;; the languages kept after the :lang pruning.
(defvar my/treesit-grammars-to-install
  '(python javascript typescript go rust json yaml markdown html css toml)
  "List of tree-sitter grammars to install.")

(defun my/install-all-treesit-grammars ()
  "Install every grammar in `my/treesit-grammars-to-install'."
  (interactive)
  (message "Installing tree-sitter grammars...")
  (dolist (lang my/treesit-grammars-to-install)
    (treesit-install-language-grammar lang))
  (message "Tree-sitter grammar installation started."))

(defun my/list-treesit-grammars ()
  "Show the status of the configured tree-sitter grammars."
  (interactive)
  (let ((buffer (get-buffer-create "*Tree-sitter Grammars*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Configured grammars:\n\n")
      (dolist (lang my/treesit-grammars-to-install)
        (insert (format "  - %s\n" lang)))
      (insert "\nInstallation status:\n\n")
      (dolist (lang my/treesit-grammars-to-install)
        (insert (format "[%s] %s\n"
                        (if (treesit-language-available-p lang) "x" "-")
                        lang))))
    (pop-to-buffer buffer)))

;;; ### Leader bindings added in this file

(my-leader
 '(("l t i" . my/install-all-treesit-grammars)
   ("l t l" . my/list-treesit-grammars)))

(provide 'my-editing)
;;; my-editing.el ends here