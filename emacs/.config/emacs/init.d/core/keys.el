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

;; ### Leader

;; Replaces Doom's `map! :leader'. Every prefix the user already knows
;; is defined here so the rest of the configuration can bind into it.
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for the leader key (SPC).")

(define-key global-map (kbd "SPC") my-leader-map)

;; Evil's own normal/visual/motion state maps shadow the global map: if we
;; only define SPC there, Evil buffers never see the leader (and SPC falls
;; back to scroll-down). Bind the leader in every state Evil is likely to be
;; in, mirroring Doom's `general' setup.


;; Restore Vim's default tab navigation (works on the visible tab bar),
;; rather than a custom command.
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "g t") #'tab-bar-switch-to-next-tab)
  (define-key evil-normal-state-map (kbd "g T") #'tab-bar-switch-to-prev-tab))

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
  ;; Bind SPC as leader in Evil states.
  (dolist (state-map '(evil-normal-state-map evil-visual-state-map evil-motion-state-map))
    (define-key (symbol-value state-map) (kbd "SPC") my-leader-map))
  (add-hook 'after-make-frame-functions (lambda (frame) (evil-mode) (dolist (sm '(evil-normal-state-map evil-visual-state-map evil-motion-state-map)) (define-key (symbol-value sm) (kbd "SPC") my-leader-map))))

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
  (which-key-idle-delay 0.2)
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


;; general-override-mode-map (installed by general/evil-collection) sits in
;; emulation-mode-map-alists and can shadow the leader in Evil buffers.
(with-eval-after-load 'general
  (when (boundp 'general-override-mode-map)
    (define-key general-override-mode-map (kbd "SPC") my-leader-map)))

;; Safeguard: SPC must be the leader everywhere, regardless of Evil/general
;; overriding maps.
(with-eval-after-load 'evil
  (define-key global-map (kbd "SPC") my-leader-map)
  (dolist (sm '(evil-normal-state-map evil-visual-state-map evil-motion-state-map))
    (define-key (symbol-value sm) (kbd "SPC") my-leader-map))
  (when (boundp 'general-override-mode-map)
    (define-key general-override-mode-map (kbd "SPC") my-leader-map))
)

;; Ensure every major mode keeps SPC=leader in its local keymap.

(defun my/bind-leader-for-current-buffer ()
  "Bind SPC to the leader keymap in the current buffer."
  (unless (or (minibufferp)
              (derived-mode-p 'minibuffer-mode 'minibuffer-inactive-mode))
    (local-set-key (kbd "SPC") my-leader-map)))
(add-hook 'after-change-major-mode-hook #'my/bind-leader-for-current-buffer)
(my/bind-leader-for-current-buffer)


;; Extra Doom-ish leader bindings.
(my-leader
 '(("SPC" . execute-extended-command)
   (":"   . execute-extended-command)
   (";"   . comment-dwim)
   (","   . consult-buffer)
   ("/"   . consult-ripgrep)
   ("u"   . universal-argument)
   ("w w" . other-window)
   ("w o" . other-window)
   ("w 0" . delete-window)
   ("b d" . kill-current-buffer)
   ("f b" . consult-buffer)
   ("f s" . save-buffer)
   ("f S" . save-some-buffers)
   ("q a" . kill-emacs)
   ("t n" . tab-bar-new-tab)
   ("t b" . tab-bar-switch-to-tab)
   ("h h" . describe-key)))

;; Ensure every major mode keeps SPC=leader in its local keymap.
(defun my/bind-leader-for-current-buffer ()
  "Bind SPC to the leader keymap in the current buffer."
  (unless (or (minibufferp)
              (derived-mode-p 'minibuffer-mode 'minibuffer-inactive-mode))
    (local-set-key (kbd "SPC") my-leader-map)))
(add-hook 'after-change-major-mode-hook #'my/bind-leader-for-current-buffer)
(my/bind-leader-for-current-buffer)
