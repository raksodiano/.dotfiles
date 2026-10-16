;;; elfeed.el -*- lexical-binding: t; -*-
;;; elfeed.el --- RSS (split out)

;;; Code:

(unless (boundp (quote my/assets-dir)) (defvar my/assets-dir (expand-file-name "assets" my/config-dir)))

(my/create-directories)

(use-package elfeed
  :custom
  (elfeed-search-filter "@7-days-ago +unread")
  (elfeed-db-directory "~/.elfeed")
  :config
  ;; No line numbers in feed lists or entries (like treemacs/vterm).
  (add-hook 'elfeed-search-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              ;; Wrap long titles instead of cutting them at the panel edge.
              (setq-local truncate-lines nil)))
  (add-hook 'elfeed-show-mode-hook (lambda () (display-line-numbers-mode -1)))
  ;; Vanilla windows: entries replace the list, `q' goes back.
  ;; Entry readability, as in Doom (+rss-elfeed-wrap-h).
  (add-hook 'elfeed-show-mode-hook #'my/elfeed-wrap-h)
  (defun my/elfeed-wrap-h ()
    "Wrap entry text for readability."
    (setq-local truncate-lines nil
                shr-use-fonts nil
                shr-width 85))
  ;; Doom's evil bindings for elfeed.
  (with-eval-after-load 'evil
    (evil-define-key 'normal elfeed-search-mode-map
      "n" #'next-line
      "p" #'previous-line
      "q" #'kill-current-buffer
      "r" #'revert-buffer
      (kbd "M-RET") #'elfeed-search-browse-url)
    (evil-define-key 'normal elfeed-show-mode-map
      "n" #'elfeed-show-next
      "p" #'elfeed-show-prev
      "gc" #'my/elfeed-copy-link))
  (defun my/elfeed-copy-link ()
    "Copy the current entry link to clipboard (Doom's +rss/copy-link)."
    (interactive)
    (let ((link (elfeed-entry-link elfeed-show-entry)))
      (when link
        (kill-new link)
        (message "Copied %s to clipboard" link)))))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed/elfeeds.org" my/assets-dir)))
  (elfeed-org))

(require 'elfeed-org nil t)

;; Embedded download functionality
(defconst my/elfeed-download-base-dir (expand-file-name "~/Downloads/Elfeed/")
  "Base directory for downloaded content.")

(defun elfeed-download--ensure-directory (dir)
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun elfeed-download--sanitize-filename (filename)
  (replace-regexp-in-string "[^a-zA-Z0-9-_. ]" "_" filename))

(defun elfeed-download-youtube (url title)
  (let ((download-dir (expand-file-name "youtube/" my/elfeed-download-base-dir)))
    (elfeed-download--ensure-directory download-dir)
    (start-process "yt-dlp" "*yt-dlp*" "yt-dlp"
                   "-o" (concat download-dir "%(title)s.%(ext)s") url)))

(defun elfeed-download-article (url title)
  (let* ((download-dir (expand-file-name "articles/" my/elfeed-download-base-dir))
         (safe-title (elfeed-download--sanitize-filename title))
         (pdf-file (concat download-dir safe-title ".pdf")))
    (elfeed-download--ensure-directory download-dir)
    (start-process "playwright-pdf" "*pdf-gen*" "node"
                   (expand-file-name "save-article.js" (expand-file-name "~/.config/scripts"))
                   url pdf-file)))

(defun elfeed-download-current-entry ()
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (when entries
      (let* ((entry (if (listp entries) (car entries) entries))
             (url (elfeed-entry-link entry))
             (title (elfeed-entry-title entry)))
        (cond
         ((string-match-p "youtube\\.com\\|youtu\\.be" url)
          (elfeed-download-youtube url title))
         (t
          (elfeed-download-article url title)))
        (elfeed-untag entry 'unread)
        (elfeed-tag entry 'read)
        (elfeed-search-update-entry entry)
        (message "Downloaded and marked: %s" title)
        (forward-line 1)))))

(with-eval-after-load 'elfeed
  (define-key elfeed-search-mode-map "d" #'elfeed-download-current-entry)
  (define-key elfeed-search-mode-map "O" #'elfeed-search-browse-url))

(use-package elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup))

(setq elfeed-tube-mpv-mpv-options '("--force-window=yes"))

(add-hook 'elfeed-show-mode-hook #'visual-fill-column-mode)

;; Periodic fetch.
(run-at-time nil (* 60 60) #'elfeed-update)

(my-leader
 '(("d e" . elfeed)
   ("d u" . elfeed-update)
   ("d v" . elfeed-tube-mpv)))

(provide 'my-elfeed)
;;; elfeed.el ends here
