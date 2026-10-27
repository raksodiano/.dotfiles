;;; undo.el -*- lexical-binding: t; -*-
;;; undo.el --- Undo limits and persistent undo history  -*- lexical-binding: t; -*-

;;; Commentary:
;; undo-fu raises the undo limits and gives a reliable redo under Meow;
;; `u' (meow-undo) is pointed at undo-fu, and C-r in normal state runs
;; the matching redo. undo-fu-session persists the undo tree across
;; sessions, so hours of prose edits survive an Emacs crash or restart;
;; the session files live in the XDG cache, never next to the files
;; being edited.

;;; Code:

(use-package undo-fu
  :custom
  (undo-limit (* 64 1024 1024))
  (undo-strong-limit (* 128 1024 1024)))

(with-eval-after-load 'meow
  ;; `meow-undo' executes `meow--kbd-undo' through `read-kbd-macro', so
  ;; it needs a key sequence rather than a command symbol. Bind that
  ;; sequence in the normal-state map to undo-fu.
  (setq meow--kbd-undo "C-/")
  (define-key meow-normal-state-keymap (kbd "C-/") #'undo-fu-only-undo))

(use-package undo-fu-session
  :custom
  (undo-fu-session-directory
   (expand-file-name "undo-fu-session" my/emacs-cache))
  (undo-fu-session-compression 'gzip))
(undo-fu-session-global-mode +1)

(provide 'my-undo)
;;; undo.el ends here
