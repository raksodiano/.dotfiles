;;; elfeed.el -*- lexical-binding: t; -*-
;;; elfeed.el --- RSS (split out)

;;; Code:

(unless (boundp (quote my/assets-dir)) (defvar my/assets-dir (expand-file-name "assets" my/config-dir)))

(my/create-directories)

(use-package elfeed
  :custom
  (elfeed-search-filter "@7-days-ago +unread")
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
  ;; Doom's elfeed bindings. Both modes run in Meow motion state, so
  ;; their local maps are not shadowed by the normal-state keys.
  (define-key elfeed-search-mode-map (kbd "n") #'next-line)
  (define-key elfeed-search-mode-map (kbd "p") #'previous-line)
  (define-key elfeed-search-mode-map (kbd "q") #'kill-current-buffer)
  (define-key elfeed-search-mode-map (kbd "r") #'revert-buffer)
  (define-key elfeed-search-mode-map (kbd "u") #'elfeed-update)
  (define-key elfeed-search-mode-map (kbd "v") #'elfeed-tube-mpv)
  (define-key elfeed-search-mode-map (kbd "M-RET") #'my/elfeed-open-entry)
  (define-key elfeed-show-mode-map (kbd "n") #'elfeed-show-next)
  (define-key elfeed-show-mode-map (kbd "p") #'elfeed-show-prev)
  (define-key elfeed-show-mode-map (kbd "v") #'elfeed-tube-mpv)
  (define-key elfeed-show-mode-map (kbd "M-RET") #'my/elfeed-open-entry)
  (define-key elfeed-show-mode-map (kbd "g c") #'my/elfeed-copy-link)
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
  (define-key elfeed-search-mode-map "O" #'my/elfeed-open-entry)
;; `v' plays the entry with mpv in both views.
  (define-key elfeed-search-mode-map "v" #'elfeed-tube-mpv)
  (define-key elfeed-show-mode-map "v" #'elfeed-tube-mpv))

(use-package elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup))

;; Route YouTube entries to mpv; the rest browses normally.
(defconst my/elfeed-youtube-rx "youtube\\.com\\|youtu\\.be"
  "Regexp matching YouTube video URLs.")

(defun my/elfeed-youtube-entry-p (entry)
  "Non-nil when ENTRY's link is a YouTube video URL."
  (when-let* ((url (elfeed-entry-link entry)))
    (string-match-p my/elfeed-youtube-rx url)))

(defun my/elfeed-open-entry ()
  "Open the entry under point.
YouTube entries play in mpv, others browse normally."
  (interactive)
  (let* ((entry (cond
                 ((eq major-mode 'elfeed-show-mode) elfeed-show-entry)
                 (t (car (elfeed-search-selected 'ignore-region))))))
    (cond
     ((my/elfeed-youtube-entry-p entry)
      (elfeed-tube-mpv (point)))
     ((eq major-mode 'elfeed-show-mode)
      (elfeed-show-visit))
     (t
      (elfeed-search-browse-url)))))

(defun my/elfeed-show-visit-a (orig &optional secondary)
  "Route YouTube entries to mpv; visit the rest as usual."
  (if (and elfeed-show-entry (my/elfeed-youtube-entry-p elfeed-show-entry))
      (elfeed-tube-mpv (point))
    (funcall orig secondary)))

(defun my/elfeed-search-browse-url-a (orig)
  "Route YouTube entries under point to mpv when browsing from search."
  (let ((entry (car (elfeed-search-selected 'ignore-region))))
    (if (my/elfeed-youtube-entry-p entry)
        (elfeed-tube-mpv (point))
      (funcall orig))))

;; RET follows entry links via `push-button' in the show buffer.
(defun my/elfeed-push-button-a (orig &optional pos use-mouse-action)
  "Route the button under POS to mpv when it is the entry's YouTube link."
  (let* ((at (cond
              ((integerp pos) pos)
              ((eventp pos) (or (posn-point (event-start pos)) (point)))
              (t (point))))
         (url (get-text-property at 'shr-url)))
    (if (and (derived-mode-p 'elfeed-show-mode)
             url (string-match-p my/elfeed-youtube-rx url))
        (elfeed-tube-mpv (point))
      (funcall orig pos use-mouse-action))))

;; Fallback for any other browse-url call from an elfeed view.
(defun my/elfeed-browse-url-a (orig url &rest args)
  "Send YouTube URLs browsed from elfeed to mpv."
  (if (and (stringp url)
           (derived-mode-p 'elfeed-show-mode 'elfeed-search-mode)
           (string-match-p my/elfeed-youtube-rx url))
      (if (and elfeed-show-entry
               (equal (elfeed-entry-link elfeed-show-entry) url))
          (elfeed-tube-mpv (point))
        (start-process "mpv" nil "mpv" "--force-window=yes" url))
    (apply orig url args)))

;; Load it before the routing advices can fire.
(require 'elfeed-tube-mpv nil t)

(advice-add 'elfeed-show-visit :around #'my/elfeed-show-visit-a)
(advice-add 'elfeed-search-browse-url :around #'my/elfeed-search-browse-url-a)
(advice-add 'push-button :around #'my/elfeed-push-button-a)
(advice-add 'browse-url :around #'my/elfeed-browse-url-a)
(with-eval-after-load 'shr
  (advice-add 'shr-browse-url :around #'my/elfeed-shr-browse-url-a))

(add-hook 'elfeed-show-mode-hook #'visual-fill-column-mode)

;; Periodic fetch.
(run-at-time nil (* 60 60) #'elfeed-update)

(my-leader
 '(("o e" . elfeed)))

(provide 'my-elfeed)
;;; elfeed.el ends here
