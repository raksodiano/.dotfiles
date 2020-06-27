;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf elfeed
      :straight t
      ;; :config
      ;; (elfeed-set-max-connections 32)
      )

(leaf elfeed-org
      :straight t
      :config
      (progn
        (elfeed-org)
        (setq rmh-elfeed-org-files (list (concat org-dir "feeds.org")))))

(leaf elfeed-goodies
      :straight t)

(elfeed-goodies/setup)

(defun rakso/custom-elfeed-sort (a b)
  (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
         (b-tags (format "%s" (elfeed-entry-tags b)))
         (a-title (elfeed-feed-title (elfeed-entry-feed a)))
         (b-title (elfeed-feed-title (elfeed-entry-feed b))))
    (if (string= a-tags b-tags)
        (if (string= a-title b-title)
            (< (elfeed-entry-date b) (elfeed-entry-date a))
          (string< b-title a-title))
      (string< a-tags b-tags))))

(setf elfeed-search-sort-function #'rakso/custom-elfeed-sort)
;; (global-set-key (kbd "C-c r") 'elfeed)

(defun rakso/elfeed-current-entry ()
  (cond ((eq major-mode 'elfeed-show-mode)
         elfeed-show-entry)
        ((eq major-mode 'elfeed-search-mode)
         (elfeed-search-selected t))))

(bind-keys :prefix-map rakso/app-elfeed
           :prefix "C-c A r"
           ("r" . elfeed))

(provide 'config)
;;; config.el ends here
