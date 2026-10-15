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
  (evil-emacs-state-modes
   (append '(calendar-mode
             dired-mode
             gdb-mode
             git-commit-mode
             grep-mode
             help-mode
             info-mode
             lisp-interaction-mode
             special-mode
             term-mode
             text-mode
             woman-mode)
           evil-emacs-state-modes))
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

(define-key global-map (kbd "SPC") my-leader-map)

(defun my-leader (bindings)
  "Bind BINDINGS, a quoted alist of (KEY . COMMAND), in `my-leader-map'.

KEY is a string that may contain spaces (used as separators, Doom
style); spaces are removed before binding, so \"o p\" binds the two
key sequence `o', `p'. The token \"SPC\" denotes the leader key itself."
  (declare (indent defun))
  (dolist (binding bindings)
    (let ((key (replace-regexp-in-string " +" "" (car binding))))
      (define-key my-leader-map (kbd key) (cdr binding)))))

;; NOTE: BINDINGS must be quoted. A literal dotted pair in argument
;; position is evaluated as a function call, so (\"o -\" . #'dired-jump)
;; would try to call the string \"o -\".
(my-leader
 '(("SPC" . consult-buffer)
   ("o -"  . dired-jump)
   ("o t"  . project-switch-project)
   ("o p"  . project-switch-project)
   ("o f"  . find-file)
   ("o r"  . recentf)))

;;; ### which-key

;; Built into Emacs 31; configured to appear as quickly as in Doom.
(eval-after-load 'which-key
  (setq which-key-idle-delay 0.2
        which-key-max-display-length 30))

(provide 'my-keys)
;;; my-keys.el ends here