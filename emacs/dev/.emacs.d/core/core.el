;;; core.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(setq-default
 backup-directory-alist (list (cons ".*" backup-dir))
 auto-save-list-file-prefix backup-dir
 auto-save-file-name-transforms `((".*" , backup-dir t))
 tramp-backup-directory-alist backup-tramp-dir)

(setq-default
 auto-save-list-file-name (concat cache-dir "autosave")
 semanticdb-default-save-directory (concat cache-dir "semanticdb")
 savehist-file (concat cache-dir "history")
 eshell-directory-name (concat cache-dir "eshell" ))

(setq-default
 server-auth-dir cache-server-dir)

(setq-default
 tramp-auto-save-directory tramp-dir
 tramp-persistency-file-name (concat tramp-dir "tramp")
 tramp-persistency-file-name (concat tramp-dir "tramp-persistency.el"))

(setq-default
 tramp-auto-save-directory cache-tramp-dir)

(setq-default
 url-cache-directory cache-url-dir
 url-configuration-directory (concat cache-url-dir "url"))

(setq custom-file (concat etc-dir "custom.el"))
(load custom-file t t)

(setq-default
 shared-game-score-directory shared-game-score-dir
 gamegrid-user-score-file-directory shared-game-score-dir)

(setq-default
 url-configuration-directory url-dir)

(setq history-length 1000
      history-delete-duplicates t
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring
                                      search-ring
                                      regexp-search-ring))

(savehist-mode t)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Fix unicode errors
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")

(setq-default frame-title-format (list "Emacs"))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      large-file-warning-threshold 100000000
      ;; Scroll
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.02
      scroll-down-aggressively 0.02)

(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(setq-default indent-tabs-mode nil
              tab-width 2)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(toggle-indicate-empty-lines)
(delete-selection-mode)
(blink-cursor-mode -1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(set-default 'truncate-lines t)
(setq visual-line-mode t)
(global-eldoc-mode -1)
(electric-pair-mode 1)
(setq-default truncate-lines nil
              global-visual-line-mode t)

(setq-default ad-redefinition-action 'accept
              apropos-do-all t
              compilation-always-kill t
              compilation-ask-about-save nil
              compilation-scroll-output t
              confirm-nonexistent-file-or-buffer t
              enable-recursive-minibuffers nil
              idle-update-delay 2
              minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
              auto-save-default nil
              auto-save-list-file-prefix nil
              make-backup-files nil
              create-lockfiles nil)

(setq-default bidi-display-reordering nil
              blink-matching-paren nil
              cursor-in-non-selected-windows nil
              display-line-numbers-width 3
              frame-inhibit-implied-resize t
              fringe-indicator-alist (delq
                                      (assq 'continuation fringe-indicator-alist)
                                      fringe-indicator-alist)
              highlight-nonselected-windows nil
              image-animate-loop t
              indicate-buffer-boundaries nil
              indicate-empty-lines nil
              max-mini-window-height 0.3
              mode-line-default-help-echo nil
              mouse-yank-at-point t
              ibuffer-use-other-window t
              resize-mini-windows 'grow-only
              show-help-function nil
              split-width-threshold 160
              uniquify-buffer-name-style 'forward
              use-dialog-box nil
              visible-cursor nil
              x-stretch-cursor nil
              jit-lock-defer-time nil
              jit-lock-stealth-nice 0.1
              jit-lock-stealth-time 0.2
              jit-lock-stealth-verbose nil
              pos-tip-internal-border-width 6
              pos-tip-border-width 1
              ring-bell-function #'ignore
              visible-bell nil)

;;
;; Load files
;;
(load-file (concat user-emacs-directory "core/core-packages.el"))

(load-directory (expand-file-name "modules/" user-emacs-directory) t)

(provide 'core)
;;; core.el ends here
