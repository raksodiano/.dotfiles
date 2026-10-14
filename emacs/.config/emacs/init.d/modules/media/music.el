;;; music.el -*- lexical-binding: t; -*-
;;; music.el --- Music / EMMS (split out)

;;; Code:

(use-package emms
  :commands emms
  :init (setq emms-source-file-default-directory (expand-file-name "~/Music"))
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
        emms-source-file-directory-tree-function
        'emms-source-file-directory-tree-find))

(use-package alert :defer t)

;; Alert with metadata.
(defun my/emms-show-alert ()
  (when (featurep 'alert)
    (when-let* ((track (emms-playlist-current-selected-track))
                (artist (emms-track-get track 'info-artist))
                (title (emms-track-get track 'info-title))
                (album (emms-track-get track 'info-album)))
      (alert (format "%s\n%s" artist album)
             :title (format " %s" title)
             :category "music"))))

(with-eval-after-load 'emms
  (add-hook 'emms-player-started-hook #'my/emms-show-alert))

;; Bindings under SPC m.
(my-leader
 '(("m m d" . emms-play-directory-tree)
   ("m m p" . emms-playlist-mode-go)
   ("m m h" . emms-shuffle)
   ("m m x" . emms-pause)
   ("m m s" . emms-stop)
   ("m m b" . emms-previous)
   ("m m n" . emms-next)))

;; Alert on org/latex export (kept with media as in Doom).
(defun my/org-export-pdf-alert (&rest _)
  (alert "Org to PDF export completed"
         :title "Org Export" :severity 'normal))

(with-eval-after-load 'ox-latex
  (advice-add 'org-latex-export-to-pdf :after #'my/org-export-pdf-alert))

;; Audio/video export alert no extra.

(provide 'my-music)
;;; music.el ends here
