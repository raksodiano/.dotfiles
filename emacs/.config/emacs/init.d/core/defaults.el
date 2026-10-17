;;; my-defaults.el -*- lexical-binding: t; -*-
;;; my-defaults.el --- Encoding, performance and general behaviour  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's config.el. These settings have no package dependencies
;; and must be loaded before anything else.

;;; Code:

;;; ### Encoding

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq highlight-escape-sequences nil)

;;; ### General behaviour

;; Recycle bin instead of hard deletes.
(setq delete-by-moving-to-trash t)

;; Trailing whitespace and whitespace-mode are Doom UI defaults; the user
;; keeps both off globally.
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (boundp 'show-trailing-whitespace)
              (setq show-trailing-whitespace nil))
            (when (boundp 'whitespace-mode)
              (whitespace-mode -1))))

;;; ### Performance

(setq gc-cons-threshold (* 16 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq comp-deferred-compilation t)
(setq comp-async-jobs-number 8)

(defun my-restore-gc-threshold ()
  "Relax the GC threshold once startup is over."
  (setq gc-cons-threshold (* 64 1024 1024)))

(add-hook 'emacs-startup-hook #'my-restore-gc-threshold)

;; gcmh relies on the thresholds set in early-init.el (gcmh-idle-delay,
;; gcmh-high-cons-threshold): once startup finishes it owns the GC,
;; dropping threshold during idle and raising right before allocations,
;; which smooths out pauses during long editing sessions.
;; NOTE: read-process-output-max is already 4MB above (LSP/elfeed).
(use-package gcmh
  :hook (emacs-startup . gcmh-mode))

;;; ### Frames and windows

(setq frame-inhibit-implied-resize t)
(setq focus-follows-mouse nil)

;; Fullscreen on startup. Doom had two lines doing the same thing via
;; initial-frame-alist and default-frame-alist; only the latter is needed.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Undecorated (frameless) frames, matching the Hyprland setup.
(add-to-list 'default-frame-alist '(undecorated . t))

;;; ### Directory scaffolding

;; Ported verbatim from Doom's config.el. Paths are unchanged: they point at
;; ~/Org, ~/Workspace and ~/Library as before.

(defvar +my/org-base-dir (expand-file-name "~/Org")
  "Base directory for Org files.")

(defvar +my/org-notes-dir (expand-file-name "notes" +my/org-base-dir)
  "Directory for Org notes.")

(defvar +my/org-agenda-dir (expand-file-name "agenda" +my/org-base-dir)
  "Directory for Org agenda files.")

(defvar +my/org-work-dir (expand-file-name "work" +my/org-notes-dir)
  "Directory for work-related notes.")

(defvar +my/org-journal-dir (expand-file-name "journal" +my/org-base-dir)
  "Directory for Org journal.")

(defvar +my/workspace-dir (expand-file-name "~/Workspace")
  "Workspace base directory.")

(defvar +my/library-dir (expand-file-name "~/Library")
  "Library base directory.")

(defvar my/directories
      (append
       (list +my/org-base-dir
             (expand-file-name "journal" +my/org-base-dir)
             (expand-file-name "journal/personal" +my/org-base-dir)
             (expand-file-name "journal/work" +my/org-base-dir)
             +my/org-agenda-dir
             +my/org-notes-dir
             (expand-file-name "notes/blog" +my/org-notes-dir)
             (expand-file-name "notes/book" +my/org-notes-dir)
             (expand-file-name "notes/personal" +my/org-notes-dir)
             +my/org-work-dir
             (expand-file-name "notes/games" +my/org-notes-dir))
       (list +my/library-dir
             +my/workspace-dir
             (expand-file-name "Books" +my/workspace-dir)
             (expand-file-name "Books/latex" +my/workspace-dir)
             (expand-file-name "Books/org" +my/workspace-dir)
             (expand-file-name "Blog" +my/workspace-dir)
             (expand-file-name "Personal" +my/workspace-dir)
             (expand-file-name "Scripts" +my/workspace-dir)
             (expand-file-name "Work" +my/workspace-dir)
             (expand-file-name "Games" +my/workspace-dir)))
  "List of directories created on demand by `my/create-directories'.")

(defun my/create-directories (&optional force)
  "Create configured directories. With FORCE, recreate even if they exist."
  (interactive "P")
  (dolist (directory my/directories)
    (when (or force (not (file-directory-p directory)))
      (make-directory directory t)
      (message "Created directory: %s" directory))))

;;; ### Dictionary

;; Ported from Doom's config.el.

(defun my/change-ispell-dictionary (lang)
  "Change the Flyspell dictionary to LANG."
  (interactive "sLanguage (e.g., es_ES or en_US): ")
  (setq ispell-dictionary lang)
  (ispell-change-dictionary lang)
  (message "Dictionary changed to %s" lang))

;; NOTE: flyspell-lazy-idle-seconds no longer exists in Emacs 31's
;; flyspell; the lazy machinery moved to flyspell-lazy. Spell checking is
;; configured per mode in core/editing.el instead.

(provide 'my-defaults)
;;; my-defaults.el ends here