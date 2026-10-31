;;; treesit.el -*- lexical-binding: t; -*-
;;; treesit.el --- Tree-sitter glue  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's `:lang ... +tree-sitter' modules and its
;; `+my/lsp-... +treesit' helpers. Only configuration, no installation of
;; system libraries.

;;; Code:

;;; ### Grammar table

(defconst my/treesit-grammar-dir
  (expand-file-name "tree-sitter" my/emacs-data)
  "Directory where tree-sitter grammars are installed.")

(defvar my/treesit-grammars-to-install
  '(python javascript typescript tsx go rust json yaml markdown markdown-inline
    php html css toml bash)
  "Tree-sitter grammars we want for this configuration.")

;; Emacs 31 ships no recipes (treesit-language-source-alist is nil), so
;; every grammar needs its upstream repository spelled out here.
(defvar my/treesit-language-sources
  '((python          "https://github.com/tree-sitter/tree-sitter-python")
    (javascript      "https://github.com/tree-sitter/tree-sitter-javascript")
    (typescript      "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
    (tsx             "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
    (go              "https://github.com/tree-sitter/tree-sitter-go")
    (rust            "https://github.com/tree-sitter/tree-sitter-rust")
    (json            "https://github.com/tree-sitter/tree-sitter-json")
    (yaml            "https://github.com/tree-sitter/tree-sitter-yaml")
    (markdown        "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
    (php             "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
    (html            "https://github.com/tree-sitter/tree-sitter-html")
    (css             "https://github.com/tree-sitter/tree-sitter-css")
    (toml            "https://github.com/tree-sitter/tree-sitter-toml")
    (bash            "https://github.com/tree-sitter/tree-sitter-bash"))
  "Upstream recipes for `treesit-language-source-alist'.")

(defun my/treesit-grammar-installed-p (lang)
  (treesit-language-available-p lang))

(defun my/install-all-treesit-grammars ()
  "Install every grammar listed in `my/treesit-grammars-to-install'."
  (interactive)
  (message "Installing tree-sitter grammars...")
  (dolist (lang my/treesit-grammars-to-install)
    (condition-case err
        (treesit-install-language-grammar lang my/treesit-grammar-dir)
      (error (message "my/treesit: could not install %s: %s"
                      lang (error-message-string err)))))
  (message "my/treesit: grammar installation started."))

(defun my/list-treesit-grammars ()
  "List configured tree-sitter grammars and their availability."
  (interactive)
  (let ((buffer (get-buffer-create "*Tree-sitter Grammars*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Configured tree-sitter grammars:\n\n")
      (dolist (lang my/treesit-grammars-to-install)
        (insert (format "  [%s] %s\n"
                        (if (treesit-language-available-p lang) "x" "-")
                        lang))))
    (pop-to-buffer buffer)))

(defun my/treesit-doctor ()
  "Report tree-sitter grammar availability."
  (interactive)
  (let ((buffer (get-buffer-create "*Tree-sitter Doctor*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Tree-sitter Doctor\n\n")
      (dolist (lang my/treesit-grammars-to-install)
        (insert (format "  [%s] %s\n"
                        (if (treesit-language-available-p lang) "x" "-")
                        lang)))
      (let ((missing (cl-loop for lang in my/treesit-grammars-to-install
                              unless (treesit-language-available-p lang)
                              collect lang)))
        (insert "\nMissing grammars:\n")
        (if missing
            (dolist (lang missing)
              (insert (format "  - %s\n" lang)))
          (insert "  (none)\n")))
      (pop-to-buffer buffer))))

;;; ### Configuration

(use-package treesit
  :ensure nil
  :config
  (setq treesit-auto-install t
        treesit-font-lock-level 4)
  ;; Make the interactive default for `treesit-install-language-grammar'
  ;; point at the XDG data directory instead of user-emacs-directory.
  (setq treesit--install-language-grammar-out-dir-history
        (list my/treesit-grammar-dir))
  (dolist (entry my/treesit-language-sources)
    (add-to-list 'treesit-language-source-alist entry))
  ;; Prefer tree-sitter major modes over the classical ones when available.
  (when (fboundp 'treesit-install-language-grammar)
    (setq major-mode-remap-alist
          (append '((python-mode . python-ts-mode)
                    (javascript-mode . javascript-ts-mode)
                    (typescript-mode . typescript-ts-mode)
                    (json-mode . json-ts-mode)
                    (css-mode . css-ts-mode)
                    (yaml-mode . yaml-ts-mode)
                    (bash-mode . bash-ts-mode)
                    (html-mode . html-ts-mode)
                    (go-mode . go-ts-mode)
                    (rust-mode . rust-ts-mode))
                  major-mode-remap-alist))))

;;; ### Leader bindings

(my-leader
 '(("l t i" . my/install-all-treesit-grammars)
   ("l t l" . my/list-treesit-grammars)
   ("l t d" . my/treesit-doctor)
   ("l t e" . treesit-ensure-installed)))

(provide 'my-treesit)
;;; treesit.el ends here
