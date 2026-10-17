;;; undo.el -*- lexical-binding: t; -*-
;;; undo.el --- Undo limits and persistent undo history  -*- lexical-binding: t; -*-

;;; Commentary:
;; undo-fu raises the undo limits and gives a reliable C-r redo under
;; Evil (Evil's own redo is a no-op without undo-tree here); Evil
;; sessions get u / C-r bound to the undo-fu pair.
;; undo-fu-session persists the undo tree across sessions, so hours of
;; prose edits survive an Emacs crash or restart; the session files
;; live in the XDG cache, never next to the files being edited.

;;; Code:

(use-package undo-fu
  :custom
  (undo-limit (* 64 1024 1024))
  (undo-strong-limit (* 128 1024 1024)))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map "u" #'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") #'undo-fu-only-redo))

(use-package undo-fu-session
  :custom
  (undo-fu-session-directory
   (expand-file-name "undo-fu-session" my/emacs-cache))
  (undo-fu-session-compression 'gzip))
(undo-fu-session-global-mode +1)

(provide 'my-undo)
;;; undo.el ends here
