;;; early-init.el -*- lexical-binding: t; -*-
;;; early-init.el --- Package bootstrap, native comp & GC tuning

;;; Commentary:
;; Runs before the main init. Every line here hits startup, so keep it short.
;; Sets up archives, native compilation, GC, and keeps runtime state in XDG dirs.

;;; Code:

;; evil-collection needs this before Evil loads.
(setq evil-want-keybinding nil)
;; Hide chrome before any frame exists — no menu/tool/scroll bar flash.
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; ### Package archives

;; nongnu moved: elpa.elpa.org is gone.
(defconst my/package-archives
  '(("gnu"    . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/"))
  "Package archives used by `package-refresh-contents'.")

(require 'package)
(unless (equal package-archives my/package-archives)
  (setq package-archives my/package-archives))

;; Native comp built-in since Emacs 30.
(setq package-always-enable-native-comp t)

;; Don't refresh index on start; use `M-x package-refresh-all'.
(setq package-enable-at-startup nil)

;;; ### Garbage collection

;; High during load, relaxed after startup.
(setq gc-cons-threshold (* 16 1024 1024))

;; gcmh takes over GC after startup.
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 128 1024 1024))

;;; ### Package location

;; Put packages in XDG data dir, not ~/.emacs.d.
(setq user-install-directory
      (expand-file-name "elpa" (or (getenv "XDG_DATA_HOME")
                                   (expand-file-name "~/.local/share")))
      package-user-dir user-install-directory)

;;; ### Runtime state

;; Config is a stow symlink to git — keep runtime writes in XDG dirs.
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
      native-comp-async-report-warnings-errors 'silent)

;; Lock files, auto-saves, and other state — keep out of the repo.
(setq auto-save-directory (expand-file-name "auto-save" my/emacs-cache)
      lockfile-directory my/emacs-cache
      url-configuration-directory (expand-file-name "url" my/emacs-cache)
      tramp-auto-save-directory (expand-file-name "tramp" my/emacs-cache)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" my/emacs-cache)
      tramp-persistency-file-name (expand-file-name "tramp-persistency.el" my/emacs-cache)
      recentf-save-file (expand-file-name "recentf.eld" my/emacs-cache)
      project-list-file (expand-file-name "projects.eld" my/emacs-cache)
      savehist-file (expand-file-name "savehist.eld" my/emacs-cache)
      save-place-file (expand-file-name "saveplace" my/emacs-cache)
      bookmark-default-file (expand-file-name "bookmarks.eld" my/emacs-data)
      org-id-locations-file (expand-file-name ".org-id-locations" my/emacs-cache)
      abbrev-file-name (expand-file-name "abbrev_defs" my/emacs-cache)
      eshell-directory-name (expand-file-name "eshell" my/emacs-cache)
      transient-history-file (expand-file-name "transient/history.el" my/emacs-cache)
      transient-levels-file (expand-file-name "transient/levels.el" my/emacs-cache)
      transient-values-file (expand-file-name "transient/values.el" my/emacs-cache)
      calc-settings-file (expand-file-name "calc.el" my/emacs-data)
      diary-file (expand-file-name "diary" my/emacs-data)
      nsm-settings-file (expand-file-name "network-security.eld" my/emacs-data)
      package-quickstart-file (expand-file-name "package-quickstart.el" my/emacs-cache)
      custom-theme-directory (file-name-as-directory (expand-file-name "themes" my/emacs-data))
      TeX-auto-global (file-name-as-directory (expand-file-name "auctex" my/emacs-cache))
      lsp-server-install-dir (expand-file-name "lsp" my/emacs-data)
      image-dired-dir (file-name-as-directory (expand-file-name "image-dired" my/emacs-cache))
      eww-bookmarks-directory (file-name-as-directory (expand-file-name "eww" my/emacs-data))
      elfeed-db-directory (expand-file-name "elfeed" my/emacs-data)
      treesit-extra-load-path (list (expand-file-name "tree-sitter" my/emacs-data))
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" my/emacs-cache)
      projectile-frecency-file (expand-file-name "projectile-frecency.eld" my/emacs-cache)
      projectile-session-directory (file-name-as-directory (expand-file-name "projectile-sessions" my/emacs-data))
      dirvish-cache-dir (file-name-as-directory (expand-file-name "dirvish" my/emacs-cache))
      ellama-community-prompts-file (expand-file-name "ellama/community-prompts.csv" my/emacs-cache)
      ellama-blueprint-global-dir (expand-file-name "ellama/blueprints" my/emacs-data)
      ellama-skills-global-path (expand-file-name "ellama/skills" my/emacs-data)
      ellama-tools-irreversible-project-trust-store-file (expand-file-name "ellama/irreversible-trust.el" my/emacs-data)
      ellama-tools-dlp-audit-log-file (expand-file-name "ellama/dlp-audit.jsonl" my/emacs-data)
      nov-save-place-file (expand-file-name "nov-places" my/emacs-data)
      mc/list-file (expand-file-name "mc-lists.el" my/emacs-cache)
      request-storage-directory (file-name-as-directory (expand-file-name "request" my/emacs-cache))
      emms-directory (file-name-as-directory (expand-file-name "emms" my/emacs-cache))
      lsp-session-file (expand-file-name ".lsp-session-v1" my/emacs-cache)
      backup-directory-alist `((".*" . ,(expand-file-name "backups" my/emacs-cache)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save" my/emacs-cache) t)))

;;; ### Credentials

;; Every credential used inside Emacs (gptel, agent-shell, aidermacs,
;; ERC, jabber, ...) lives in ~/.secrets/, one file per service; see
;; modules/tools/secrets.el. auth-source only searches the files named
;; here, so the directory is scanned: a new file is picked up without
;; editing this list. Runs before any module, because gptel, agent-shell
;; and aidermacs all look their keys up through auth-source.
(setq auth-sources
      (append (let ((dir (expand-file-name ".secrets" "~")))
                (when (file-directory-p dir)
                  (directory-files dir t "\\`[^.]")))
              auth-sources))

;; Point package.el at real package dir.
(setq package-gnupghome-dir (expand-file-name "gnupg" package-user-dir))

;; Activate packages so their dirs hit load-path.
(package-initialize)

(provide 'early-init)
;;; early-init.el ends here