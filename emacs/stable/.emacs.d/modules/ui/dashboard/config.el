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

  :hook (after-init . dashboard-setup-startup-hook)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
        (concat "GNU Emacs " emacs-version " kernel "
                (car (split-string (shell-command-to-string "uname -r") "-")) " "
                (car (split-string (shell-command-to-string "uname -m") "-")))
        dashboard-page-separator "\n\f\n")

  (setq dashboard-center-content t
        dashboard-startup-banner 'logo)

  (setq show-week-agenda-p t
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))

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
      (">" dashboard-next-section "Next Section")
      ("<" dashboard-previous-section "Previous Section")
      ("r" dashboard-goto-recent-files "Recent Files")
      ("m" dashboard-goto-bookmarks "Bookmarks")
      ("p" dashboard-goto-projects "Projects")
      ("a" dashboard-goto-agenda "Agenda")
      ("H" browse-homepage "Browse Homepage" :exit t)
      ("R" restore-session "Restore Previous Session" :exit t)
      ("E" dashboard-edit-config "Open custom file" :exit t)
      ("C-g" nil "quit"))
    (bind-keys :map dashboard-mode-map
               ("h" . dashboard-hydra/body)
               ("?" . dashboard-hydra/body))))

(provide 'config)
;;; config.el ends here
