;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package emms
  :init
  (setq emms-directory (concat cache-dir "emms/"))
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (emms-all)
  (setq emms-seek-seconds 5)
  (setq emms-player-list '(emms-player-mpd))
  (setq emms-info-functions '(emms-info-mpd))
  (setq emms-player-mpd-server-name "localhost"))

(setq emms-player-list '(emms-player-mpd))
(setq emms-info-functions '(emms-info-mpd))

(defun mpd/start-music-daemon ()
  "Start MPD, connects to it and syncs the metadata cache."
  (interactive)
  (shell-command "mpd")
  (mpd/update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))
;; (global-set-key (kbd "s-m c") 'mpd/start-music-daemon)

(defun rakso/music-browser ()
  (interactive)
  (delete-other-windows)
  (emms-smart-browse))

(defun rakso/music-current-album ()
  (cdr (assoc 'info-album
              (emms-playlist-current-selected-track))))

(defun rakso/music-current-artist ()
  (cdr (assoc 'info-artist
              (emms-playlist-current-selected-track))))

(defun rakso/music-current-title ()
  (cdr (assoc 'info-title
              (emms-playlist-current-selected-track))))

(defun rakso/entity-escape (string)
  (s-replace-all '(("&" . "&amp;") ("<" . "&lt;") (">" "&gt;")) string))

(defun rakso/music-notification-message (album artist)
  "Format a nice notification message about the current song.
Include the album if it's non-nil. Escape some entities in the
message, since Pango (?) can't seem to parse them."
  (if album
      (concat "by: <i>" (hrs/entity-escape artist) "</i>\n"
              "from: <i>" (hrs/entity-escape album) "</i>")
    (concat "by: <i>" (hrs/entity-escape artist) "</i>")))

(defun rakso/notify-current-song ()
  (let* ((album (hrs/music-current-album))
         (artist (hrs/music-current-artist))
         (title (hrs/music-current-title))
         (message (hrs/music-notification-message album artist)))
    (hrs/notify-send title message)))

(add-hook 'emms-player-started-hook '(lambda () (rakso/notify-current-song)))

(bind-keys :prefix-map rakso/app-music
           :prefix "C-c A m"
           ("s" . emms-smart-browse)
           ("r" . emms-player-mpd-update-all-reset-cache)
           ("b" . rakso/music-browser))

(provide 'config)
;;; config.el ends here
