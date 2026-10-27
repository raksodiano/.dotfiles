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

;; Keybindings in calibredb-search-mode. The mode runs in Meow motion
;; state, so its local map is not shadowed by the normal-state keys.
(with-eval-after-load 'calibredb
  (define-key calibredb-search-mode-map (kbd "RET") #'calibredb-find-file)
  (define-key calibredb-search-mode-map (kbd "?") #'calibredb-dispatch)
  (define-key calibredb-search-mode-map (kbd "a") #'calibredb-add)
  (define-key calibredb-search-mode-map (kbd "d") #'calibredb-remove)
  (define-key calibredb-search-mode-map (kbd "j") #'calibredb-next-entry)
  (define-key calibredb-search-mode-map (kbd "k") #'calibredb-previous-entry)
  (define-key calibredb-search-mode-map (kbd "l") #'calibredb-open-file-with-default-tool)
  (define-key calibredb-search-mode-map (kbd "s") #'calibredb-set-metadata-dispatch)
  (define-key calibredb-search-mode-map (kbd "S") #'calibredb-switch-library)
  (define-key calibredb-search-mode-map (kbd "q") #'calibredb-search-quit))

(provide 'my-books)
;;; books.el ends here
