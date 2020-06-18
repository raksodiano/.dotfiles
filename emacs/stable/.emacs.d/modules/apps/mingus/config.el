;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package mingus
  :straight t)

(defun rakso/music-browser ()
  (interactive)
  (delete-other-windows)
  (mingus))

(global-set-key (kbd "C-c B") 'rakso/music-browser)

;; (defun rakso/music-current-album ()
;;   (cdr (assoc 'info-album
;;               (emms-playlist-current-selected-track))))

;; (defun rakso/music-current-artist ()
;;   (cdr (assoc 'info-artist
;;               (emms-playlist-current-selected-track))))

;; (defun rakso/music-current-title ()
;;   (cdr (assoc 'info-title
;;               (emms-playlist-current-selected-track))))

;; (defun rakso/entity-escape (string)
;;   (s-replace-all '(("&" . "&amp;") ("<" . "&lt;") (">" "&gt;")) string))

;; (defun rakso/music-notification-message (album artist)
;;   "Format a nice notification message about the current song.
;; Include the album if it's non-nil. Escape some entities in the
;; message, since Pango (?) can't seem to parse them."
;;   (if album
;;       (concat "by: <i>" (hrs/entity-escape artist) "</i>\n"
;;               "from: <i>" (hrs/entity-escape album) "</i>")
;;     (concat "by: <i>" (hrs/entity-escape artist) "</i>")))

;; (defun rakso/notify-current-song ()
;;   (let* ((album (hrs/music-current-album))
;;          (artist (hrs/music-current-artist))
;;          (title (hrs/music-current-title))
;;          (message (hrs/music-notification-message album artist)))
;;     (hrs/notify-send title message)))

;; (add-hook 'emms-player-started-hook '(lambda () (hrs/notify-current-song)))

(provide 'config)
;;; config.el ends here
