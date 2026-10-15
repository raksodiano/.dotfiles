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
  (setq all-the-icons-scale-factor 0.95
        all-the-icons-default-adjust -0.2))

;;; ### Fonts

;; Same fonts as the Doom config: Cascadia Mono NF for code, Alegreya for
;; prose (variable-pitch in Org).
(when (display-graphic-p)
  (setq doom-font (font-spec :family "CaskaydiaMono NF" :size 15)
        doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
        doom-big-font (font-spec :family "CaskaydiaMono NF" :size 22))

  ;; The historical Doom config named the family "Cascadia Mono NF", but
  ;; this host's installed font is spelled "CaskaydiaMono NF". Apply it to
  ;; the `default' face instead of only to the dead `doom-font' variable.
  (set-face-attribute 'default nil :font doom-font)
  (set-frame-font doom-font t t)

  (set-face-attribute 'fixed-pitch nil :font doom-font)
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
  (centaur-tabs-set-icons t)
  :config
  ;; Doom only shows the tab bar once a real file is opened, not at
  ;; startup (the dashboard would otherwise show an empty tab bar).
  (defun my/centaur-tabs-maybe-enable ()
    "Enable centaur-tabs once a file-visiting buffer exists."
    (unless centaur-tabs-mode
      (when (cl-some #'buffer-file-name (buffer-list))
        (centaur-tabs-mode +1))))
  (add-hook 'find-file-hook #'my/centaur-tabs-maybe-enable)
  (add-hook 'after-save-hook #'my/centaur-tabs-maybe-enable))

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
        persp-initial-frame-workspace nil
        persp-auto-resume-time 0)
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

;; Doom's (indent-guides): on in code buffers. Loaded after the theme
;; above, otherwise it cannot derive its faces. GUI only: face
;; auto-derivation needs a real background, which terminals (and batch
;; runs) don't provide.
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method (if (display-graphic-p) 'bitmap 'character))
  (highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (display-graphic-p)
                (highlight-indent-guides-mode 1)))))

;;; ### Line numbers and cursor

;; Ported from Doom: relative line numbers everywhere, with the ordinary
;; per-major-mode exceptions below.
(setq-default display-line-numbers-type 'relative
              display-line-numbers t)
(global-display-line-numbers-mode +1)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Cursor shape and color per Evil state, gruvbox-tinted like Doom.
(with-eval-after-load 'evil
  (setq evil-normal-state-cursor '(box "#b8bb26")
        evil-insert-state-cursor '(bar "#fb4934")
        evil-visual-state-cursor '(hbar "#fabd2f")
        evil-replace-state-cursor '(hbar "#fb4934")
        evil-motion-state-cursor '(box "#83a598")
        evil-emacs-state-cursor '(box "#d3869b")
        evil-operator-state-cursor '(hbar "#b8bb26")))

;;; ### Doom eye-candy modules

;; hl-todo: TODO/FIXME/NOTE screaming in code, as in Doom.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; vi-tilde-fringe: Vim-like ~ past end of buffer, as in Doom.
(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))

;; vc-gutter +pretty: git marks in the fringe, refreshed with magit.
;; :demand so the global mode is on from startup, like Doom.
(use-package diff-hl
  :demand t
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; solaire-mode: Doom's depth cue, dimming sidebars and popups.
(use-package solaire-mode
  :config
  (solaire-global-mode 1))

;; rainbow-delimiters in code, as Doom's doom module does.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
  ;; Doom centred the whole dashboard; section shortcut letters stay
  ;; visible so every section has an easy access key (p/m/r/a...).
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-items '((projects . 5) (bookmarks . 5) (agenda . 8) (recents . 5)))
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
  ;; Every section shows its icon before the name (nerd-icons when
  ;; available, all-the-icons otherwise; hidden in terminal).
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  ;; Section shortcut letters are always shown (dashboard only shows
  ;; them — and only defines the jump keys — for non-empty sections).
  ;; The overlay renames below keep the underlying "Name:" text intact,
  ;; so section search/jump keeps working.
  (dashboard-heading-shorcut-format "")
  (dashboard-item-names
   '(("Projects:" . "Projects: (p)")
     ("Agenda for today:" . "Agenda for today: (a)")
     ("Agenda for the coming week:" . "Agenda for the coming week: (a)")
     ("Bookmarks:" . "Bookmarks: (m)")
     ("Recent Files:" . "Recent Files: (r)")))
  :config
  (dashboard-setup-startup-hook)
  ;; Jump commands that work even when a section is empty (dashboard only
  ;; generates its own when the section has items).
  (defun my/dashboard-jump-to-section (label)
    "Move point to the dashboard section headed LABEL."
    (goto-char (point-min))
    (if (search-forward label nil t)
        (progn (forward-line 1) (back-to-indentation))
      (goto-char (point-min))))
  (defun my/dashboard-jump-to-projects ()
    "Jump to the dashboard Projects section."
    (interactive)
    (my/dashboard-jump-to-section "Projects:"))
  (defun my/dashboard-jump-to-bookmarks ()
    "Jump to the dashboard Bookmarks section."
    (interactive)
    (my/dashboard-jump-to-section "Bookmarks:"))
  (defun my/dashboard-jump-to-agenda ()
    "Jump to the dashboard Agenda section (week or day view)."
    (interactive)
    (goto-char (point-min))
    (if (or (search-forward "Agenda for the coming week:" nil t)
            (progn (goto-char (point-min))
                   (search-forward "Agenda for today:" nil t)))
        (progn (forward-line 1) (back-to-indentation))
      (goto-char (point-min))))
  (defun my/dashboard-jump-to-recents ()
    "Jump to the dashboard Recent Files section."
    (interactive)
    (my/dashboard-jump-to-section "Recent Files:"))
  ;; Plain bindings cover non-Evil states; Evil normal-state bindings win
  ;; over evil's own keys (e.g. "p" pastes) inside the dashboard.
  (define-key dashboard-mode-map (kbd "p") #'my/dashboard-jump-to-projects)
  (define-key dashboard-mode-map (kbd "a") #'my/dashboard-jump-to-agenda)
  (define-key dashboard-mode-map (kbd "m") #'my/dashboard-jump-to-bookmarks)
  (define-key dashboard-mode-map (kbd "r") #'my/dashboard-jump-to-recents)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      (kbd "p") #'my/dashboard-jump-to-projects
      (kbd "a") #'my/dashboard-jump-to-agenda
      (kbd "m") #'my/dashboard-jump-to-bookmarks
      (kbd "r") #'my/dashboard-jump-to-recents)))

;; Doom showed the dashboard instead of *scratch* on startup.
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      initial-buffer-choice nil)
;; Ensure the dashboard takes focus after persp-mode/workspaces have had a
;; chance to (attempt to) restore their saved state, and leave point on
;; the Projects section (shortcut "p", generated as dashboard-jump-to-projects
;; when the section renders).
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 0.3 nil
                            (lambda ()
                              (dashboard-initialize)
                              (let ((buf (get-buffer "*dashboard*")))
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (my/dashboard-jump-to-projects)))))))
          t)

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


;;; my-ui.el ends here
(setq nerd-icons-scale-factor 0.95
      nerd-icons-default-adjust -0.2)

(provide 'my-ui)
;;; my-ui.el ends here
