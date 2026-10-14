;;; treesit.el -*- lexical-binding: t; -*-
;;; treesit.el --- Tree-sitter glue  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's `:lang ... +tree-sitter' modules and its
;; `+my/lsp-... +treesit' helpers. Only configuration, no installation of
;; system libraries.

;;; Code:

;;; ### Grammar table

(defvar my/treesit-grammars-to-install
  '(python javascript typescript go rust json yaml markdown php html css toml)
  "Tree-sitter grammars we want for this configuration.")

(defun my/treesit-grammar-installed-p (lang)
  (treesit-language-available-p lang))

(defun my/install-all-treesit-grammars ()
  "Install every grammar listed in `my/treesit-grammars-to-install'."
  (interactive)
  (message "Installing tree-sitter grammars...")
  (dolist (lang my/treesit-grammars-to-install)
    (condition-case err
        (treesit-install-language-grammar lang)
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
