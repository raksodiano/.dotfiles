;;; my-keys.el -*- lexical-binding: t; -*-
;;; my-keys.el --- Modal editing and key bindings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's config.el.
;;
;; NOTE: the current Evil (1.15) renamed its variables and keymaps:
;; `evil-normal-map' is now `evil-normal-state-map', `evil-escape-delay'
;; became `evil-esc-delay', and `evil-escape-key-sequence' no longer
;; exists. The third-party `evil-escape' package restores that variable,
;; so the "kj" escape sequence the user is used to is preserved.
;;
;; The `general' package was dropped on purpose: its leader-key API is
;; gone in the current release, and plain keymaps are easier to audit
;; during a migration.

;;; Code:

;;; ### use-package defaults

(setq use-package-always-ensure t
      use-package-always-update-on-changelog nil
      use-package-always-pin nil)

;;; ### Evil

;; Doom's (evil +everywhere): the modal map replaces emacs-mode-map in
;; every buffer except the ones listed below.
(setq-default evil-move-cursor-back nil
      evil-want-fine-undo t
      evil-case-fold-search t)

;; NOTE: package variables belong in :custom, not :init. :init runs
;; before the package is loaded, so a defcustom would overwrite the value.
(use-package evil-escape
  :custom
  (evil-escape-key-sequence "kj")
  (evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))

;; NOTE: Evil 1.15 removed evil-mode-disable. Disabling Evil in a buffer
;; is now done by listing the major mode in `evil-emacs-state-modes',
;; which already covers magit-*, org-agenda-mode, ediff-mode, pdf-view-mode,
;; occur-mode, package-menu-mode, recentf-dialog-mode and the reftex modes.
;; `evil-buffer-regexps' is the equivalent for buffers matched by name.
(use-package evil
  :custom
  ;; Keep only Evil's built-in emacs state entries so the modes the user
  ;; works with daily (dired, text, org, magit, ...) become modal rather
  ;; than staying in "holy"/Emacs state.
  (evil-emacs-state-modes nil)
  (evil-default-state 'normal)

  :config
  (define-key evil-normal-state-map (kbd "<M-left>") #'evil-backward-word)
  (define-key evil-normal-state-map (kbd "<M-right>") #'evil-forward-word)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  ;; Turn Evil on. Evil has no global minor mode, so it is enabled here
  ;; and re-enabled for every new frame; evil-escape-mode does nothing
  ;; useful while evil-mode is off.
  ;; NOTE: there is no evil-set-default-keybindings in Evil 1.15; the
  ;; default bindings are installed by evil-mode itself.
  (evil-mode 1)
  (add-hook 'after-make-frame-functions #'evil-mode)

  ;; evil-collection restores Doom's "evil everywhere" behaviour: it adds
  ;; the right bindings/motion to magit, dired, org, term, help, etc.
  ;; and moves them out of pure Emacs state.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

  ;; Evil 1.15 declares evil-mode-buffers with (defvar evil-mode-buffers),
  ;; which marks it special but leaves it void, and evil-normal-post-command
  ;; reads it through evil-initializing-p on every command in Normal state:
  ;;   Error in post-command-hook (evil-normal-post-command):
  ;;   (void-variable evil-mode-buffers)
  ;; Verified present in both NonGNU ELPA and MELPA. Give it a value
  ;; until upstream fixes the declaration.
  (unless (boundp 'evil-mode-buffers)
    (setq evil-mode-buffers nil)))

;;; ### Leader

;; Replaces Doom's `map! :leader'. Every prefix the user already knows
;; is defined here so the rest of the configuration can bind into it.
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for the leader key (SPC).")

;; Doom-like modal leader: SPC opens the leader ONLY in Evil's
;; normal/visual/motion/emacs states. It is deliberately NOT bound in
;; `global-map', in local maps, or in insert state, so SPC keeps inserting
;; a space while typing (minibuffer narrowing like "find file" needs it).
(defconst my-leader-state-maps
  '(evil-normal-state-map evil-visual-state-map
    evil-motion-state-map evil-emacs-state-map)
  "Evil state maps where SPC opens `my-leader-map'.")

(with-eval-after-load 'evil
  (dolist (state-map my-leader-state-maps)
    (define-key (symbol-value state-map) (kbd "SPC") my-leader-map)))

;; Restore Vim's default tab navigation (works on the visible tab bar),
;; rather than a custom command.
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "g t") #'centaur-tabs-forward-tab)
  (define-key evil-normal-state-map (kbd "g T") #'centaur-tabs-backward-tab))

(defun my-leader (bindings)
  "Bind BINDINGS, a quoted alist of (KEY . COMMAND), in `my-leader-map'.

KEY is a string that may contain spaces (used as separators, Doom
style); spaces are removed before binding, so \"o p\" binds the two
key sequence `o', `p'. The token \"SPC\" denotes the leader key itself."
  (declare (indent defun))
  (dolist (binding bindings)
    ;; `kbd' already understands the spaces Doom uses as separators, so
    ;; "SPC ." becomes the two-key sequence <SPC> then ".".
    (define-key my-leader-map (kbd (car binding)) (cdr binding))))

;; NOTE: BINDINGS must be quoted. A literal dotted pair in argument
;; position is evaluated as a function call, so (\"o -\" . #'dired-jump)
;; would try to call the string \"o -\".
(my-leader
 '(("SPC" . consult-buffer)
   ("o -"  . dired-jump)
   ("o t"  . project-switch-project)
   ("o p"  . project-dired)
   ("o f"  . find-file)
   ("o r"  . recentf)))

;;; ### which-key

;; Built into Emacs 31; configured to appear as quickly as in Doom.
(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0)
  (which-key-max-display-length 30)
  (which-key-setup-side-window-right)
  :config
  (which-key-mode 1))


(provide 'my-keys)
;;; my-keys.el ends here

;; ### Doom-like common bindings (basic)
(my-leader
 '(("SPC" . consult-buffer)
   ("b b"  . consult-buffer)
   ("b B"  . consult-buffer-other-window)
   ("b k"  . kill-this-buffer)
   ("b K"  . (lambda () (interactive) (mapc #'kill-buffer (delq (current-buffer) (buffer-list)))))
   ("b l"  . ibuffer)
   ("b n"  . next-buffer)
   ("b p"  . previous-buffer)
   ("f f"  . find-file)
   ("f r"  . recentf-open-files)
   ("f p"  . project-find-file)
   ("g g"  . magit-status)
   ("g d"  . magit-dispatch)
   ("s b"  . consult-line)
   ("s l"  . consult-line)
   ("s s"  . consult-ripgrep)
   ("t t"  . treemacs)
   ("w /"  . split-window-right)
   ("w -"  . split-window-below)
   ("w d"  . delete-window)
   ("w m"  . delete-other-windows)
   ("q q"  . save-buffers-kill-terminal)
   ("q Q"  . (lambda () (interactive) (kill-emacs)))
   ("o -"  . dired-jump)))

;; ### Doom window bindings
(my-leader
 '(("w h" . windmove-left)
   ("w j" . windmove-down)
   ("w k" . windmove-up)
   ("w l" . windmove-right)
   ("w s" . split-window-below)
   ("w v" . split-window-right)
   ("w c" . delete-window)
   ("w d" . delete-window)
   ("w m" . delete-other-windows)
   ("w u" . winner-undo)
   ("w U" . winner-redo)))


(defun my-leader-sanity-check ()
  "Open a diagnostic buffer for SPC bindings."
  (interactive)
  (let ((buffer (get-buffer-create "*leader-check*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "major-mode: %s\n" (symbol-name major-mode)))
      (insert (format "current-local-map SPC: %S\n" (lookup-key (current-local-map) (kbd "SPC"))))
      (insert (format "global SPC: %S\n" (lookup-key global-map (kbd "SPC"))))
      (insert (format "evil-normal-state-map SPC: %S\n" (lookup-key evil-normal-state-map (kbd "SPC"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))
