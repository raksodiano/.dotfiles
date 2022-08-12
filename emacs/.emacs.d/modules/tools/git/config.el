;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(setq vc-follows-symlinks t
      find-file-visit-truename t
      vc-handled-backends nil)

(use-package magit
  :straight t
  :init
  (progn
    ;; (setq magit-git-executable "tg")
    (delete 'Git vc-handled-backends)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defadvice git-commit-commit (after delete-window activate)
      (delete-window))
    (defadvice git-commit-abort (after delete-window activate)
      (delete-window))
    (defun magit-commit-mode-init ()
      (when (looking-at "\n")
        (open-line 1))))
  :config
  (bind-keys :prefix-map rakso-magit-keys
             :prefix "C-x g"
             ("c" . magit-commit)
             ("e" . magit-ediff-resolve)
             ("g" . magit-grep)
             ("l" . magit-file-log)
             ("p" . magit-push-other)
             ("u" . magit-pull)
             ("r" . magit-rebase-interactive)
             ("s" . magit-status)
             ("x" . magit-checkout))

  (progn
    (defadvice magit-quit-window (around magit-restore-screen activate)
      (let ((current-mode major-mode))
        ad-do-it
        (when (eq 'magit-status-mode current-mode)
          (jump-to-register :magit-fullscreen))))
    (defun magit-maybe-commit (&optional show-options)
      "Runs magit-commit unless prefix is passed"
      (interactive "P")
      (if show-options
          (magit-key-mode-popup-committing)
        (magit-commit)))
    (define-key magit-mode-map "c" 'magit-maybe-commit)

    (setq magit-completing-read-function 'ivy-completing-read
          magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
          magit-status-buffer-switch-function 'switch-to-buffer
          magit-diff-refine-hunk t
          magit-rewrite-inclusive 'ask
          magit-process-find-password-functions '(magit-process-password-auth-source)
          magit-save-some-buffers t
          magit-process-popup-time 10
          magit-set-upstream-on-push 'askifnotset
          magit-refs-show-commit-count 'all
          magit-log-buffer-file-locket t)))

(use-package git-gutter
  :straight t
  :defer 1
  :config
  (bind-keys :prefix-map rakso-git-gutter
             :prefix "C-x ="
             ("g" . git-gutter)
             ("P" . git-gutter:popup-hunk)
             ("p" . git-gutter:previous-hunk)
             ("n" . git-gutter:next-hunk)
             ("s" . git-gutter:stage-hunk)
             ("r" . git-gutter:revert-hunk)
             ("SPC" . git-gutter:mark-hunk))

  (if (display-graphic-p)
      (use-package git-gutter-fringe
        :ensure t))
  (global-git-gutter-mode t)
  (setq-default fringes-outside-margins t)
  (setq indicate-empty-lines nil)
  (setq git-gutter:lighter ""
        git-gutter:handled-backends '(git hg bzr svn))
  (set-face-foreground 'git-gutter:modified "purple")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")

  (defun my-reshape-git-gutter (gutter)
    "Re-shape gutter for `ivy-read'."
    (let* ((linenum-start (aref gutter 3))
           (linenum-end (aref gutter 4))
           (target-line "")
           (target-linenum 1)
           (tmp-line "")
           (max-line-length 0))
      (save-excursion
        (while (<= linenum-start linenum-end)
          (goto-line linenum-start)
          (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                   (buffer-substring (line-beginning-position)
                                                                     (line-end-position))))
          (when (> (length tmp-line) max-line-length)
            (setq target-linenum linenum-start)
            (setq target-line tmp-line)
            (setq max-line-length (length tmp-line)))

          (setq linenum-start (1+ linenum-start))))
      ;; build (key . linenum-start)
      (cons (format "%s %d: %s"
                    (if (eq 'deleted (aref gutter 1)) "-" "+")
                    target-linenum target-line)
            target-linenum)))

  (defun my-goto-git-gutter ()
    (interactive)
    (if git-gutter:diffinfos
        (ivy-read "git-gutters:"
                  (mapcar 'my-reshape-git-gutter git-gutter:diffinfos)
                  :action (lambda (e)
                            ;; ivy9+ keep `(car e)'
                            ;; ivy8- strip the `(car e)'
                            ;; we handle both data structure
                            (unless (numberp e) (setq e (cdr e)))
                            (goto-line e)))
      (message "NO git-gutters!"))))

;; (use-package gitconfig-mode
;; :straight t
;; :mode ("/\\.?git/?config$"
;; "/\\.gitmodules$")
;; :init (add-hook 'gitconfig-mode-hook 'flyspell-mode))

;; (use-package gitignore-mode
;; :straight t
;; :mode ("/\\.gitignore$"
;; :"/\\.git/info/exclude$"
;; "/git/ignore$"))

;; (use-package gitattributes-mode
  ;; :straight t
  ;; :defer t)

(use-package git-timemachine
  :straight t
  :commands git-timemachine
  :bind (:map git-timemachine-mode
              ("c" . git-timemachine-show-current-revision)
              ("b" . git-timemachine-switch-branch)))

(use-package smerge-mode
  :straight t
  :config
  (defun enable-smerge-maybe ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode +1)))))

  (add-hook 'buffer-list-update-hook #'enable-smerge-maybe))

(provide 'config)
;;; config.el ends here
