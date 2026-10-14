;;; init.el -*- lexical-binding: t; -*-
;;; init.el --- Entry point  -*- lexical-binding: t; -*-
;;; Migrated from Doom Emacs (init.el + config.el).

;;; Commentary:
;; Everything lives in init.d/. This file only locates the directory, loads
;; the files in lexical order and sets up the few hooks that need to run
;; early. See `M-x checkdoc' conventions: files here should be short.

;;; Code:

(defconst my/config-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the Emacs configuration.")

(defconst my/init-dir
  (expand-file-name "init.d" my/config-dir)
  "Directory holding the configuration modules (init.d/core, init.d/modules).")

(defconst my/assets-dir
  (expand-file-name "assets" my/config-dir)
  "Directory holding non-Elisp assets (sounds, images, LaTeX classes, feeds).")

(defconst my/snippets-dir
  (expand-file-name "snippets" my/config-dir)
  "Directory holding personal yasnippet snippets.")

;;; ### Interactive helpers

(defun my/find-config-file (name)
  "Find configuration file NAME in the configuration directory."
  (expand-file-name (concat name ".el") my/config-dir))

;;; ### Startup order

;; init.d/ has two entries: core/ is the foundation everything else
;; builds on, modules/ holds one directory per optional domain. The load
;; order is decided here, not by the file names: a file may depend on
;; anything listed above it (core/editing.el needs my-leader from
;; core/keys.el).
(defconst my/init-files
  '(("init.d/core/defaults.el"               . my-defaults)
    ("init.d/core/keys.el"                   . my-keys)
    ("init.d/core/ui.el"                     . my-ui)
    ("init.d/core/buffers.el"                . my-buffers)
    ("init.d/core/editing.el"                . my-editing)
    ("init.d/modules/git/magit.el"           . my-magit)
    ("init.d/modules/langs/treesit.el"       . my-treesit)
    ("init.d/modules/langs/lsp.el"           . my-lsp)
    ("init.d/modules/org/org.el"             . my-org)
    ("init.d/modules/org/capture.el"         . my-org-capture)
    ("init.d/modules/org/journal.el"         . my-org-journal)
    ("init.d/modules/org/latex.el"           . my-org-latex)
    ("init.d/modules/org/hugo.el"            . my-org-hugo)
    ("init.d/modules/org/roam.el"            . my-org-roam)
    ("init.d/modules/org/writer.el"          . my-org-writer)
    ("init.d/modules/org/alerts.el"          . my-org-alerts)
    ("init.d/modules/feeds/elfeed.el"        . my-elfeed)
    ("init.d/modules/media/pdf.el"           . my-pdf)
    ("init.d/modules/media/books.el"         . my-books)
    ("init.d/modules/media/music.el"         . my-music)
    ("init.d/modules/ai/ellama.el"           . my-ellama)
    ("init.d/modules/tools/detached.el"      . my-detached)
    ("init.d/modules/tools/pass.el"          . my-pass))
  "Configuration modules as (PATH . FEATURE), in load order.

FEATURE is the symbol each module provides; `my-load-init-files' uses it
to notice a module that failed halfway through.")

(defvar my/init-verbose nil
  "When non-nil, report every file `my/load-init-files' skips.

Startup is quiet by default: a pending file is normal until its
migration phase is written, so announcing it on every start is just
noise. Run `M-x my-init-status' to see what is loaded and what is
pending.")

(defvar my/init-errors nil
  "List of (FILE . MESSAGE) for modules that did not finish loading.")

(defun my/load-init-files ()
  "Load every existing module listed in `my-init-files'.

Each entry is a (PATH . FEATURE) cons, where FEATURE is the symbol the
module provides. A file belonging to a phase that has not been written
yet is skipped silently unless `my-init-verbose' is non-nil.

NOERROR is kept on `load' deliberately: without it a module that needs a
missing package triggers a package installation, which is not something
an init file should do. A module that fails is caught afterwards by
checking that it provided its feature."
  (dolist (entry my/init-files)
    (let* ((file (car entry))
           (feature (cdr entry))
           (path (expand-file-name file my/config-dir)))
      (cond
       ((not (file-exists-p path))
        (when my/init-verbose
          (message "my/init: pending, skipping %s" file)))
       (t
        (load path nil t)
        (unless (featurep feature)
          (push (cons file "did not finish loading") my/init-errors)))))))

(my/load-init-files)

;;; ### Status

(defun my/init-status ()
  "Report which configuration modules are loaded, pending or broken."
  (interactive)
  (let ((loaded 0)
        (pending nil)
        (failed nil))
    (dolist (entry my/init-files)
      (let ((file (car entry))
            (feature (cdr entry)))
        (cond
         ((not (file-exists-p (expand-file-name file my/config-dir)))
          (push file pending))
         ((featurep feature)
          (setq loaded (1+ loaded)))
         (t
          (push file failed)))))
    (message "my/init: %d/%d modules loaded, %d pending, %d broken"
             loaded (length my/init-files) (length pending) (length failed))
    (when pending
      (message "my/init: pending: %s"
               (mapconcat #'abbreviate-file-name (reverse pending) " ")))
    (dolist (file (reverse failed))
      (message "my/init: BROKEN %s" file))
    (when my/init-errors
      (dolist (err (reverse my/init-errors))
        (message "my/init: ERROR in %s: %s" (car err) (cdr err))))))

;; Surface a broken module shortly after startup rather than at load time,
;; where the message scrolls past in an instant.
(add-hook 'emacs-startup-hook
          (lambda ()
            (when my/init-errors
              (run-with-timer 10
                              (lambda ()
                                (message "my/init: %d module(s) did not finish loading - run M-x my-init-status"
                                         (length my/init-errors)))))))

;;; ### Server

;; Doom's doom-after-init-hook, once the daemon is up.
(add-hook 'server-after-make-server-hook
          (lambda ()
            (message "my/init: Emacs server ready")))

(provide 'init)
;;; init.el ends here