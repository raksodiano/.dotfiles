;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package page-break-lines
  :straight t
  :diminish page-break-lines-mode
  :config (turn-on-page-break-lines-mode))

(use-package dashboard
  :straight t
  :functions (widget-forward
              winner-undo
              open-custom-file
              persp-load-state-from-file
              persp-get-buffer-or-null
              persp-switch-to-buffer)
  :preface
  (defvar dashboard-recover-layout-p nil)

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (dashboard-goto-recent-files)
    (delete-other-windows))

  (defun restore-session ()
    "Restore last session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (condition-case-unless-debug err
          (persp-load-state-from-file)
        (error
         (message "Error: Unable to restore last session -- %s" err)))
      (when (persp-get-buffer-or-null persp-special-last-buffer)
        (persp-switch-to-buffer persp-special-last-buffer))))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (when (and dashboard-recover-layout-p
               (bound-and-true-p winner-mode))
      (winner-undo)
      (setq dashboard-recover-layout-p nil)))

  (defun dashboard-edit-config ()
    "Open custom config file."
    (interactive)
    (quit-dashboard)
    (open-custom-file))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (funcall (local-key-binding "r")))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (funcall (local-key-binding "m")))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (funcall (local-key-binding "p")))

  (defun dashboard-goto-agenda ()
    "Go to agenda."
    (interactive)
    (funcall (local-key-binding "a")))

  :bind (:map dashboard-mode-map
              ("H" . browse-homepage)
              ("E" . dashboard-edit-config)
              ("R" . restore-session)
              ("q" . quit-dashboard))
  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
        (concat "GNU Emacs " emacs-version " kernel "
                (car (split-string (shell-command-to-string "uname -r") "-")) " "
                (car (split-string (shell-command-to-string "uname -m") "-")))
        dashboard-page-separator "\n\f\n")

  (setq show-week-agenda-p t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          ;; (registers . 5)
                          ))

  (dashboard-insert-startupify-lists)

  (setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))

  (with-eval-after-load 'hydra
    (defhydra dashboard-hydra (:color red :columns 3)
      "Help"
      ("<tab>" widget-forward "Next Widget")
      ("C-i" widget-forward "Prompt")
      ("<backtab>" widget-backward "Previous Widget")
      ("RET" widget-button-press "Press Widget" :exit t)
      ("g" dashboard-refresh-buffer "Refresh" :exit t)
      ("}" dashboard-next-section "Next Section")
      ("{" dashboard-previous-section "Previous Section")
      ("r" dashboard-goto-recent-files "Recent Files")
      ("m" dashboard-goto-bookmarks "Bookmarks")
      ("p" dashboard-goto-projects "Projects")
      ("a" dashboard-goto-agenda "Agenda")
      ("H" browse-homepage "Browse Homepage" :exit t)
      ("R" restore-session "Restore Previous Session" :exit t)
      ("E" dashboard-edit-config "Open custom file" :exit t)
      ("<f2>" open-dashboard "Open Dashboard" :exit t)
      ("q" quit-dashboard "Quit Dashboard" :exit t)
      ("C-g" nil "quit"))
    (bind-keys :map dashboard-mode-map
               ("h" . dashboard-hydra/body)
               ("?" . dashboard-hydra/body))))

(provide 'config)
;;; config.el ends here
