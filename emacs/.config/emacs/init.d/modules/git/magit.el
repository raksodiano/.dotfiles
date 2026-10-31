;;; my-magit.el -*- lexical-binding: t; -*-
;;; my-magit.el --- Magit and diff tooling  -*- lexical-binding: t; -*-
;;; Phase 1 of the migration.

;;; Commentary:
;; Ported from Doom's (magit +forge) module and the Magit section of
;; config.el. The commented-out glab/gtea/gogs/buck recipes in
;; Doom's packages.el are dropped; forge support is available through
;; magit's own `magit-auth-oauth' and the glab recipe can be added back
;; independently if needed.

;;; Code:

(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c C-g" . magit-dispatch))
  :custom
  (magit-refresh-status-buffer t)
  ;; NOTE: Doom set magit-blame-heading-format, but that variable no longer
  ;; exists in magit; the equivalent is now magit-blame-time-format.
  (magit-blame-time-format "%F %H:%M")
  :config
  ;; Refresh every magit buffer after any VCS operation, as in config.el.
  (dolist (hook '(magit-post-push-hook
                  magit-post-commit-hook
                  magit-post-merge-hook
                  magit-post-checkout-hook
                  magit-post-fetch-hook))
    (add-hook hook #'magit-refresh-all))
  (custom-set-faces
   '(magit-blame-hash ((t (:foreground "#7F7F7F"))))))

;; Side-by-side diffs through git-delta. The mode is enabled ONLY in
;; actual diff buffers (magit-diff-mode, from which the commit/status
;; diff sections also render): with the old `(magit-mode . ...)' hook
;; the status buffer itself entered magit-delta-mode, and when the
;; `delta' executable is missing every magit refresh died inside the
;; ANSI rewriting, silently leaving the status buffer without the
;; Untracked/Unstaged/Staged file sections (verified in batch with a
;; magit-status dump: only "Recent commits" survived).
(use-package magit-delta
  :hook (magit-diff-mode . my/magit-delta-maybe-enable)
  :config
  (setq magit-delta-default-options
        '("--side-by-side" "--line-numbers" "--navigate"))
  (defun my/magit-delta-maybe-enable ()
    "Enable `magit-delta-mode' when the delta binary is present."
    (when (executable-find "delta")
      (magit-delta-mode +1))))

;; Ported from config.el: disable delta on very large diffs instead of
;; waiting for a huge ANSI escape sequence to be parsed.
(defcustom dan/magit-delta-point-max 50000
  "Maximum diff buffer size `magit-delta' will process."
  :group 'magit-delta
  :type 'natnum)

(advice-add 'magit-delta-call-delta-and-convert-ansi-escape-sequences
            :around
            (lambda (orig-fun &rest args)
              "Disable delta if the buffer is too large."
              (if (<= (point-max) dan/magit-delta-point-max)
                  (apply orig-fun args)
                (magit-delta-mode -1))))

;;; ### Version control interface

;; Doom's (vc-gutter +pretty). vc-git is built into Emacs and registers
;; itself through `vc-handled-backends'; there is no minor mode to globalise.
(eval-after-load 'vc-git
  (lambda ()
    (add-to-list 'vc-handled-backends 'Git)))

(provide 'my-magit)
;;; my-magit.el ends here