;;; early-init.el -*- lexical-binding: t; -*-
;;; early-init.el --- Package bootstrap and performance tuning  -*- lexical-binding: t; -*-
;;; Migrated from Doom Emacs. Runs before the main init file.

;;; Commentary:
;; This file is loaded before the package system finishes loading, so it is
;; the right place to register archives, enable native compilation and tune
;; the garbage collector. Keep it small: every line here is on the critical
;; path of every startup.

;;; Code:

;; evil-collection requires this before Evil loads.
(setq evil-want-keybinding nil)
;; Hide chrome before any frame is created, so the menu/tool/scroll bars
;; never flash in a GUI session.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;;; ### Package archives

;; nongnu: elpa.elpa.org no longer resolves, packages now live at /nongnu.
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
                   (expand-file-name "auto-save-list" my/emacs-cache)
                   (expand-file-name "url" my/emacs-cache)
                   (expand-file-name "url/cache" my/emacs-cache)
                   (expand-file-name "eshell" my/emacs-cache)
                   (expand-file-name "transient" my/emacs-cache)
                   (expand-file-name "auctex" my/emacs-cache)
                   (expand-file-name "image-dired" my/emacs-cache)
                   (expand-file-name "dirvish" my/emacs-cache)
                   (expand-file-name "ellama" my/emacs-cache)
                   (expand-file-name "request" my/emacs-cache)
                   (expand-file-name "emms" my/emacs-cache)
                   (expand-file-name "persp-confs" my/emacs-data)
                   (expand-file-name "themes" my/emacs-data)
                   (expand-file-name "lsp" my/emacs-data)
                   (expand-file-name "elfeed" my/emacs-data)
                   (expand-file-name "eww" my/emacs-data)
                   (expand-file-name "tree-sitter" my/emacs-data)
                   (expand-file-name "ellama/blueprints" my/emacs-data)
                   (expand-file-name "ellama/skills" my/emacs-data)
                   (expand-file-name "projectile-sessions" my/emacs-data)
                   (expand-file-name "gnupg" package-user-dir)))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Native compilation cache.
(setq native-comp-cache-dir (expand-file-name "eln-cache" my/emacs-cache)
      native-comp-eln-load-path
      (list (expand-file-name "eln-cache" my/emacs-cache)
            (or (getenv "EMACS_LOAD_PATH") ""))
      ;; Async compilation chatter (e.g. third-party "function not known
      ;; to be defined" notes) must not pop up as warnings; real errors
      ;; still surface in *Async-native-compile-log*.
      native-comp-async-report-warnings-errors 'silent)

;; Lock files and automatic saves.
(setq auto-save-directory (expand-file-name "auto-save" my/emacs-cache)
      lockfile-directory my/emacs-cache
      ;; url.el defaults to user-emacs-directory/url, which would pollute
      ;; the repository. Keep cookies, history and cache under XDG cache.
      url-configuration-directory (expand-file-name "url" my/emacs-cache)
      tramp-auto-save-directory (expand-file-name "tramp" my/emacs-cache)
      auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" my/emacs-cache)
      tramp-persistency-file-name
      (expand-file-name "tramp-persistency.el"
                       (expand-file-name "tramp" my/emacs-cache))
      recentf-save-file
      (expand-file-name "recentf.eld" my/emacs-cache)
      project-list-file
      (expand-file-name "projects.eld" my/emacs-cache)
      savehist-file
      (expand-file-name "savehist.eld" my/emacs-cache)
      save-place-file
      (expand-file-name "saveplace" my/emacs-cache)
      ;; Anything below defaults to user-emacs-directory via
      ;; locate-user-emacs-file and would pollute the repository.
      bookmark-default-file
      (expand-file-name "bookmarks.eld" my/emacs-data)
      org-id-locations-file
      (expand-file-name ".org-id-locations" my/emacs-cache)
      abbrev-file-name
      (expand-file-name "abbrev_defs" my/emacs-cache)
      eshell-directory-name
      (expand-file-name "eshell" my/emacs-cache)
      transient-history-file
      (expand-file-name "transient/history.el" my/emacs-cache)
      transient-levels-file
      (expand-file-name "transient/levels.el" my/emacs-cache)
      transient-values-file
      (expand-file-name "transient/values.el" my/emacs-cache)
      ;; Other built-in state that defaults to user-emacs-directory.
      calc-settings-file
      (expand-file-name "calc.el" my/emacs-data)
      diary-file
      (expand-file-name "diary" my/emacs-data)
      nsm-settings-file
      (expand-file-name "network-security.eld" my/emacs-data)
      package-quickstart-file
      (expand-file-name "package-quickstart.el" my/emacs-cache)
      ;; User-created custom themes are data, not cache, but they must not
      ;; live in the stowed config directory either.
      custom-theme-directory
      (file-name-as-directory (expand-file-name "themes" my/emacs-data))
      ;; AUCTeX extracts macro information into `TeX-auto-global'.
      TeX-auto-global
      (file-name-as-directory (expand-file-name "auctex" my/emacs-cache))
      ;; lsp-mode downloads server binaries here (not package data, so it
      ;; belongs under XDG data rather than the repository).
      lsp-server-install-dir
      (expand-file-name "lsp" my/emacs-data)
      ;; image-dired and eww are not configured elsewhere, but if invoked
      ;; they must not create directories in the repository.
      image-dired-dir
      (file-name-as-directory (expand-file-name "image-dired" my/emacs-cache))
      eww-bookmarks-directory
      (file-name-as-directory (expand-file-name "eww" my/emacs-data))
      elfeed-db-directory
      (expand-file-name "elfeed" my/emacs-data)
      ;; Tree-sitter: `treesit-install-language-grammar' defaults to
      ;; user-emacs-directory/tree-sitter when OUT-DIR is nil.
      treesit-extra-load-path
      (list (expand-file-name "tree-sitter" my/emacs-data))
      ;; Projectile (used by treemacs-projectile and consult-dir).
      projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" my/emacs-cache)
      projectile-frecency-file
      (expand-file-name "projectile-frecency.eld" my/emacs-cache)
      projectile-session-directory
      (file-name-as-directory
       (expand-file-name "projectile-sessions" my/emacs-data))
      ;; Dirvish previews and thumbnails are regenerable cache.
      dirvish-cache-dir
      (file-name-as-directory (expand-file-name "dirvish" my/emacs-cache))
      ;; Ellama: downloaded prompts are cache; blueprints, skills, trust
      ;; store and audit log are user data.
      ellama-community-prompts-file
      (expand-file-name "ellama/community-prompts.csv" my/emacs-cache)
      ellama-blueprint-global-dir
      (expand-file-name "ellama/blueprints" my/emacs-data)
      ellama-skills-global-path
      (expand-file-name "ellama/skills" my/emacs-data)
      ellama-tools-irreversible-project-trust-store-file
      (expand-file-name "ellama/irreversible-trust.el" my/emacs-data)
      ellama-tools-dlp-audit-log-file
      (expand-file-name "ellama/dlp-audit.jsonl" my/emacs-data)
      ;; Built-in/lazy packages whose defaults point at
      ;; user-emacs-directory.
      nov-save-place-file
      (expand-file-name "nov-places" my/emacs-data)
      mc/list-file
      (expand-file-name "mc-lists.el" my/emacs-cache)
      request-storage-directory
      (file-name-as-directory (expand-file-name "request" my/emacs-cache))
      ;; EMMS is installed even though this configuration does not use it.
      emms-directory
      (file-name-as-directory (expand-file-name "emms" my/emacs-cache))
      ;; LSP session file must not pollute the repository.
      lsp-session-file
      (expand-file-name ".lsp-session-v1" my/emacs-cache)
      ;; Backup files (~) must not be generated in the repository.
      backup-directory-alist
      `((".*" . ,(expand-file-name "backups" my/emacs-cache)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save" my/emacs-cache) t)))

;; package.el computes this from `user-emacs-directory' when it loads, so it
;; must be pointed at the real package directory explicitly.
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

;; Activate the installed packages: without this their directories never
;; reach load-path and use-package cannot find them.
(package-initialize)

(provide 'early-init)
;;; early-init.el ends here