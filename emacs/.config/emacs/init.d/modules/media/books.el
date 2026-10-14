;;; books.el -*- lexical-binding: t; -*-
;;; books.el --- Ebooks / calibre (split out)

;;; Code:

(use-package nov
  :mode "\\.epub\\'"
  :config
  (setq nov-unzip-program (or (executable-find "bsdtar") (executable-find "unzip"))
        nov-unzip-args '("-xC" directory "-f" filename)))

(use-package calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir +my/library-dir
        calibredb-db-dir (expand-file-name "metadata.db" +my/library-dir)
        calibredb-library-alist (list +my/library-dir)
        calibredb-format-all-the-icons t))

;; Keybindings in calibredb-search-mode.
(with-eval-after-load 'calibredb
  (evil-define-key 'normal calibredb-search-mode-map
    (kbd "RET") #'calibredb-find-file
    "?" #'calibredb-dispatch
    "a" #'calibredb-add
    "d" #'calibredb-remove
    "j" #'calibredb-next-entry
    "k" #'calibredb-previous-entry
    "l" #'calibredb-open-file-with-default-tool
    "s" #'calibredb-set-metadata-dispatch
    "S" #'calibredb-switch-library
    "q" #'calibredb-search-quit))

(provide 'my-books)
;;; books.el ends here
