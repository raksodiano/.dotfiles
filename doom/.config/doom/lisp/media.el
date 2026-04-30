;;; media.el --- PDF, Music, Epub configuration  -*- lexical-binding: t; -*-

(use-package! pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init
  (add-hook 'pdf-view-mode-hook #'pdf-view-auto-slice-minimap)
  :config
  (require 'pdf-occur)
  (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
                                        (blink-cursor-mode -1)
                                        (doom-modeline-mode -1)))

(use-package! nov
  :defer t
  :mode "\\.epub\\'"
  :config
  (setq nov-unzip-program (executable-find "bsdtar")
        nov-unzip-args '("-xC" directory "-f" filename)))

(use-package! calibredb
  :defer t
  :commands calibredb
  :config
  (setq calibredb-root-dir (expand-file-name "~/Library")
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist (list (expand-file-name "~/Library"))
        calibredb-format-all-the-icons t))

(use-package! emms
  :commands emms
  :init
  (setq emms-source-file-default-directory (expand-file-name "~/Music"))
  :config
  (emms-all)
  (emms-default-players)
  (emms-mode-line-mode 1)
  (emms-playing-time-mode 1)

  (setq emms-track-description-function
        (lambda (track)
          (let ((artist (emms-track-get track 'info-artist))
                (title (emms-track-get track 'info-title)))
            (if (and artist title)
                (format "%s - %s" artist title)
              (file-name-nondirectory (emms-track-name track))))))

  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

  (custom-set-faces
   '(emms-browser-artist-face ((t (:foreground "#ECEFF4" :height 1.1))))
   '(emms-browser-album-face ((t (:foreground "#88C0D0" :height 1.0))))
   '(emms-browser-track-face ((t (:foreground "#A3BE8C" :height 1.0))))
   '(emms-playlist-track-face ((t (:foreground "#D8DEE9" :height 1.0))))
   '(emms-playlist-selected-face ((t (:foreground "#BF616A" :weight bold))))))

  (defun emms-cover-art-path ()
    "Return the path of the cover art for the current track."
    (let* ((track (emms-playlist-current-selected-track))
           (path (emms-track-get track 'name))
           (dir (file-name-directory path))
           (cover-files (directory-files dir nil ".*\\(jpg\\|png\\|jpeg\\)$")))
      (when cover-files
        (concat dir (car cover-files)))))

  (defvar my/emms-default-cover (expand-file-name "~/.config/doom/images/kmix.svg"))

  (defun my/emms-extract-cover (track)
    "Extract embedded cover art from TRACK to a temporary file, if it exists. Returns path or nil."
    (let ((file (emms-track-get track 'name))
          (output (make-temp-file "emms-cover-" nil ".jpg")))
      (when (and file (string-match-p "\\.mp3\\|\\.flac\\|\\.m4a" file))
        (let ((exit-code (call-process "ffmpeg" nil nil nil
                                       "-y"
                                       "-i" file
                                       "-an"
                                       "-vcodec" "copy"
                                       output)))
          (if (and (file-exists-p output)
                   (= exit-code 0))
               output
             nil))))

  (defun my/emms-show-alert ()
    "Show alert with music metadata and cover if available."
    (when (featurep 'alert)
      (when-let* ((track (emms-playlist-current-selected-track))
                  (artist (emms-track-get track 'info-artist))
                  (title (emms-track-get track 'info-title))
                  (album (emms-track-get track 'info-album)))
        (let ((cover (or (my/emms-extract-cover track)
                         my/emms-default-cover)))
          (alert (format "%s\n%s" artist album)
                 :title (format " %s" title)
                 :icon cover
                 :category "music")))))

  (add-hook 'emms-player-started-hook #'my/emms-show-alert))

(map! :leader
      (:prefix ("m" . "music/EMMS")
       :desc "Play at directory tree"   "d" #'emms-play-directory-tree
       :desc "Go to emms playlist"      "p" #'emms-playlist-mode-go
       :desc "Shuffle"                  "h" #'emms-shuffle
       :desc "Emms pause track"         "x" #'emms-pause
       :desc "Emms stop track"          "s" #'emms-stop
       :desc "Emms play previous track" "b" #'emms-previous
       :desc "Emms play next track"     "n" #'emms-next))

(map! :map calibredb-search-mode-map
      :n "RET" #'calibredb-find-file
      :n "?" #'calibredb-dispatch
      :n "a" #'calibredb-add
      :n "d" #'calibredb-remove
      :n "j" #'calibredb-next-entry
      :n "k" #'calibredb-previous-entry
      :n "l" #'calibredb-open-file-with-default-tool
      :n "s" #'calibredb-set-metadata-dispatch
      :n "S" #'calibredb-switch-library
      :n "q" #'calibredb-search-quit)

(use-package! alert
  :defer t
  :custom
  (alert-default-style (if IS-LINUX 'libnotify 'osx-notifier)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (alert "Welcome, Master" :title "Doom Emacs Awaits" :severity 'normal)))

(defun my/org-export-pdf-alert (&rest _args)
  (alert "Org to PDF export completed" :title "Org Export" :severity 'normal))

(advice-add 'org-latex-export-to-pdf :after #'my/org-export-pdf-alert)

(defun my/latex-compilation-alert (filename)
  (alert "LaTeX to PDF compilation completed" :title "Latex Compile" :severity 'normal))

(add-hook 'TeX-after-compilation-finished-functions #'my/latex-compilation-alert)

(provide '+my-media)
;;; media.el ends here
