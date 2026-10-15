;;; my-ui.el -*- lexical-binding: t; -*-
;;; my-ui.el --- Theme, modeline, tabs, fonts and window chrome  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's config.el and Doom's :ui module. Doom's noctalia theme
;; is dropped in favour of gruvbox; every other visual preference (fonts,
;; modeline options, tab bar, line numbers, blink cursor) is preserved.

;;; Code:

;;; ### Theme

;; gruvbox-material is not published on any current archive; the base
;; gruvbox-theme is, and it is the theme the user asked for. The theme
;; must be loaded before anything that customises faces.
(require 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)
(setq gruvbox-style-diagnostics 'fancy)

;; Icons for buffer names, mode lines and the like.
(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 1.5))

;;; ### Fonts

;; Same fonts as the Doom config: Cascadia Mono NF for code, Alegreya for
;; prose (variable-pitch in Org).
(when (display-graphic-p)
  (setq doom-font (font-spec :family "Cascadia Mono NF" :size 15)
        doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
        doom-big-font (font-spec :family "Cascadia Mono NF" :size 22))

  (set-face-attribute 'mode-line nil
                      :height 130 :inherit 'variable-pitch)
  (set-face-attribute 'mode-line-inactive nil
                      :height 120 :inherit 'variable-pitch))

;;; ### Modeline

;; doom-modeline is a standalone package with the same entry point as Doom's,
;; so the mode toggles used elsewhere keep working.
(use-package doom-modeline
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-percent-position t)
  (doom-modeline-lsp t)
  (doom-modeline-workspace-name t)
  :config
  (doom-modeline-mode +1))

;;; ### Tabs

;; Doom's (tabs +centaur +workspace).
(use-package centaur-tabs
  :custom
  (centaur-tabs-style "alternate")
  (centaur-tabs-height 28)
  (centaur-tabs-set-bar 'under)
  :config
  (centaur-tabs-mode +1))

;;; ### Workspaces

;; Doom's (workspaces +layouts): persisted, per-buffer window
;; configurations.
;; NOTE: persp-keymap-save-environment and friends are set in :config,
;; not :custom: persp-mode has no defcustom for them, so :custom would
;; only define the variables here.
(use-package persp-mode
  :custom
  ;; The default is (persp-confs/ user-emacs-directory), which is a symlink
  ;; into the dotfiles git repository. Keep workspace state out of it.
  (persp-save-dir (expand-file-name "persp-confs" my/emacs-data))
  :config
  ;; persp-mode does NOT create this directory; every save or workspace
  ;; switch fails with "No such file or directory" until it exists.
  (unless (file-directory-p persp-save-dir)
    (make-directory persp-save-dir t))
  (setq persp-keymap-save-environment t
        persp-initial-frame-workspace nil)
  (setq-default persp-keymap '(C-c C-w)
                persp-show-branches t)
  (persp-mode +1)

  ;; To save its state, persp creates a temporary invisible frame and
  ;; calls persp-delete-other-windows on it while the selected window is
  ;; still in the main frame. Emacs 30 made `delete-other-windows' signal
  ;; when ROOT is not an ancestor of the selected window, and persp
  ;; catches it and complains on every save:
  ;;   [persp-mode] Warning: Can not delete-other-windows --
  ;;   (error "Specified root is not an ancestor of specified window")
  ;; persp ends up returning nil for that case anyway, so skipping the
  ;; call up front is behaviourally identical and only removes the noise.
  ;; NOTE: advice-add, not defadvice: defadvice is broken in Emacs 31.1
  ;; ("Eager macro-expansion failure: (wrong-type-argument listp :around)").
  (defun my/persp-delete-other-windows--around (orig-fun &optional frame win)
    "Skip ORIG-FUN when the target window lives in another frame."
    (let ((target (or (and (window-live-p win) (window-frame win))
                     frame)))
      (if (and target
               (frame-live-p target)
               (not (eq target (selected-frame))))
          nil
        (funcall orig-fun frame win))))

  (advice-add 'persp-delete-other-windows :around
              #'my/persp-delete-other-windows--around))

;; Ported from Doom's config.el: ensure a trailing newline in buffers that
;; live in the persisted window configuration directory. Doom used
;; defadvice!; advice-add is the plain-Elisp equivalent.
;;
;; `basic-save-buffer' may be called with a buffer argument, so the advice
;; has to accept one; `file-name-directory' always returns a trailing
;; slash, hence file-name-as-directory on the comparison.
(defun my-persp-autosave--add-breakline (&rest _args)
  "Append a newline to persisted buffers before they are saved."
  (let ((file (buffer-file-name)))
    (when (and file
               (/= (point-max) (point-min))
               (/= (char-after (1- (point-max))) ?\n)
               (string= (file-name-directory file)
                        (file-name-as-directory persp-save-dir)))
      (save-excursion
        (goto-char (point-max))
        (insert ?\n)))))

(advice-add 'basic-save-buffer :before #'my-persp-autosave--add-breakline)

;;; ### Indentation guides

;; Loaded after the theme above, otherwise it cannot derive its faces.
(eval-after-load 'highlight-indent-guides
  (setq highlight-indent-guides-method (if (display-graphic-p) 'bitmap 'character)
        highlight-indent-guides-responsive 'top))

;;; ### Line numbers and cursor

(setq-default display-line-numbers-mode nil)
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Doom's +org provides org-hide-leading-stars with level 4.
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local org-startup-indented t)
            (when (fboundp 'org-hide-leading-stars)
              (org-hide-leading-stars 4))))

;; Tildes in the fringe past end-of-buffer. vi-frm is no longer published on
;; any archive; display-buffer-alist is what Doom's doom-first-input-hook
;; tweak used, so keep that behaviour.
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (boundp 'display-buffer-alist)
                       (assoc 'mode display-buffer-alist))
              (setcdr (assoc 'mode display-buffer-alist) 'other))))

(blink-cursor-mode 1)

;;; ### Scroll and wrap

(setq scroll-margin 8
      scroll-conservatively 65
      smooth-scroll t)

;; Doom's word-wrap: visual line mode globally, with long prose buffers
;; wrapped at a readable width.
(use-package visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :config
  (global-visual-line-mode t))

;;; ### Splash screen

(when (display-graphic-p)
  (setq fancy-splash-image
        (expand-file-name "images/gnu_color.png" my/assets-dir)))

;;; ### Dashboard

;; Doom's doom-dashboard is no longer published on any archive, but it was
;; only a layer over the `dashboard' package, so the layout is rebuilt
;; here with the sections Doom showed: banner, greeting, projects,
;; bookmarks, recent files and version.
;; NOTE: this version of dashboard composes widgets through
;; `dashboard-startupify-list'; the old dashboard-set-* helpers are gone.
(use-package dashboard
  :custom
  ;; Doom centred the whole dashboard and hid the per-item shortcuts.
  (dashboard-center-content t)
  (dashboard-show-shortcuts nil)
  (dashboard-items '((projects . 5) (bookmarks . 5) (recents . 5)))
  (dashboard-items-default-length 8)
  ;; Doom's greeting, from its config.el.
  (dashboard-banner-logo-title
   (format "Welcome Home, %s." (or (getenv "USER") (user-login-name))))
  ;; Doom's banner image.
  (dashboard-startup-banner
   (expand-file-name "images/gnu_color.png" my/assets-dir))
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-navigator
     dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-newline
     dashboard-insert-footer))
  :config
  (dashboard-setup-startup-hook))

;; Doom showed the dashboard instead of *scratch* on startup.
(setq inhibit-startup-screen t)

;;; ### Window borders and transparency

;; Remove the menu bar and the tool bar: with a frameless window (see
;; `undecorated' in 00-defaults.el) they are the only bars left at the
;; top of the frame. This is Doom's remove-top-bar module.
;;
;; NOTE: these must be 0, never nil. With nil the window accessors such
;; as `window-right-divider-width' return nil, and doom-modeline subtracts
;; them unguarded:
;;   (- (window-pixel-width) (window-scroll-bar-width)
;;      (window-right-divider-width) ... (car (window-fringes)))
;; which fails with "Wrong type argument: integerp, nil" and takes the
;; whole mode line down. Zero looks the same on screen and stays an
;; integer.
(setq internal-border-width 0
      internal-border-width-horizontal 0
      vertical-border-width 0
      right-divider-width 0)

(defun my/hide-chrome ()
  "Hide the menu bar, the tool bar and the internal borders."
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)
    (when (display-graphic-p)
      ;; tool-bar-mode only affects the first frame reliably.
      (dolist (frame (frame-list))
        (set-frame-parameter frame 'menu-bar-lines 0)
        (set-frame-parameter frame 'tool-bar-lines 0)
        (set-frame-parameter frame 'internal-border-width 0)
        (set-frame-parameter frame 'vertical-border-width 0)))))

(add-hook 'after-make-frame-functions #'my/hide-chrome)
(add-hook 'emacs-startup-hook #'my/hide-chrome)

;; Transparent terminal: keep the default face background unset in TTY
;; frames so the compositor shows through.
;; In a terminal, leave the default background unspecified so the
;; terminal emulator's own background shows through.
(unless (display-graphic-p)
  (set-face-background 'default nil))

;; Alpha 94/97, matching the previous Doom config; uncomment to restore.
;; (add-to-list 'default-frame-alist '(alpha . (94 . 97)))

;;; ### Comment and indentation defaults

(setq comment-tabs t)
(setq-default tab-width 2
              indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq tab-always-indent nil)

(provide 'my-ui)
;;; my-ui.el ends here