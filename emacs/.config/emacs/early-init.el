;;; early-init.el -*- lexical-binding: t; -*-
;;; early-init.el --- Package bootstrap and performance tuning  -*- lexical-binding: t; -*-
;;; Migrated from Doom Emacs. Runs before the main init file.

;;; Commentary:
;; This file is loaded before the package system finishes loading, so it is
;; the right place to register archives, enable native compilation and tune
;; the garbage collector. Keep it small: every line here is on the critical
;; path of every startup.

;;; Code:

;;; ### Package archives

;; NOTE: elpa.elpa.org no longer resolves and elpa.nongnu.org/packages was
;; moved to elpa.nongnu.org/nongnu. Both verified on 2026-10-14.
(defconst my/package-archives
  '(("gnu"    . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  "Package archives used by `package-refresh-contents'.")

;; Set these before package.el loads its own defaults.
;; package-archives does not exist until package.el is loaded.
(require 'package)
(unless (equal package-archives my/package-archives)
  (setq package-archives my/package-archives))

;; Emacs 30+ compiles installed packages to native code automatically. This is
;; what Doom used to provide via el-pa; we get it from package.el instead.
(setq package-always-enable-native-comp t)

;; Do not re-download the index on every startup; `M-x package-refresh-all'
;; is explicit now.
(setq package-enable-at-startup nil)

;;; ### Garbage collection

;; Large thresholds while loading the config, then relaxed once the startup
;; finishes (see `my-restore-gc-threshold' in init.d/core/defaults.el).
(setq gc-cons-threshold (* 16 1024 1024))

;; gcmh is provided by the gcmh package, but the variable is safe to set here
;; because it is read at runtime.
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 128 1024 1024))

;;; ### Package location

;; Keep package installs under XDG data instead of ~/.emacs.d.
;; package-user-dir must be set, not just user-install-directory: package.el
;; computes it once at load time.
(setq user-install-directory
      (expand-file-name "elpa" (or (getenv "XDG_DATA_HOME")
                                   (expand-file-name "~/.local/share")))
      package-user-dir user-install-directory)

;;; ### Runtime state

;; The configuration directory is a symlink into a git repository (it is
;; managed with stow), so anything Emacs writes at runtime must be kept
;; out of it. By default these land in the config directory and show up
;; as hundreds of untracked files.
(defconst my/xdg-cache
  (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache"))
  "XDG cache directory.")

(defconst my/emacs-cache
  (expand-file-name "emacs" my/xdg-cache)
  "Cache directory used by this Emacs configuration.")

(defconst my/emacs-data
  (expand-file-name "emacs" (or (getenv "XDG_DATA_HOME")
                                (expand-file-name "~/.local/share")))
  "Data directory used by this Emacs configuration.")

(dolist (dir (list my/emacs-cache
                   (expand-file-name "eln-cache" my/emacs-cache)
                   (expand-file-name "auto-save" my/emacs-cache)
                   (expand-file-name "tramp" my/emacs-cache)
                   (expand-file-name "persp-confs" my/emacs-data)))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Native compilation cache.
(setq native-comp-cache-dir (expand-file-name "eln-cache" my/emacs-cache)
      native-comp-eln-load-path
      (list (expand-file-name "eln-cache" my/emacs-cache)
            (or (getenv "EMACS_LOAD_PATH") "")))

;; Lock files and automatic saves.
(setq auto-save-directory (expand-file-name "auto-save" my/emacs-cache)
      lockfile-directory my/emacs-cache
      tramp-auto-save-directory (expand-file-name "tramp" my/emacs-cache))

;; Activate the installed packages: without this their directories never
;; reach load-path and use-package cannot find them.
(package-initialize)

(provide 'early-init)
;;; early-init.el ends here