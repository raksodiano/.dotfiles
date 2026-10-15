;;; vterm.el -*- lexical-binding: t; -*-
;;; vterm.el --- Fully-featured terminal (Doom's :term vterm)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Doom binds "o t" to +vterm/toggle: a bottom popup running vterm in the
;; current project. Rebuilt here with side windows (no Doom popup system).

;;; Code:

(defvar my/vterm-buffer-name "*vterm*"
  "Buffer used by `my/vterm-toggle'.")

(defun my/vterm--directory ()
  "Directory for a new vterm: project root when inside one."
  (let ((proj (and (fboundp 'project-current) (project-current nil))))
    (if proj
        (if (fboundp 'project-root) (project-root proj) default-directory)
      default-directory)))

(use-package vterm
  :commands (vterm my/vterm-toggle)
  :custom
  (vterm-shell (or (getenv "SHELL") "/bin/bash"))
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t)
  :config
  ;; No line numbers in the terminal (like treemacs).
  (add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode -1)))
  ;; The native module needs cmake + libvterm; compile on first use.
  (unless (require 'vterm-module nil t)
    (when (fboundp 'vterm-module-compile)
      (vterm-module-compile)
      (require 'vterm-module nil t))))

;;;###autoload
(defun my/vterm-toggle ()
  "Toggle a vterm popup at the bottom, like Doom's `+vterm/toggle'.

Shows `my/vterm-buffer-name' in a bottom side window (25% height),
creating it in the current project root when inside one; hides it
again when visible."
  (interactive)
  (require 'vterm nil t)
  (let ((buf (get-buffer my/vterm-buffer-name)))
    (if (and buf (get-buffer-window buf))
        (delete-window (get-buffer-window buf))
      (let ((default-directory (my/vterm--directory)))
        (setq buf (get-buffer-create my/vterm-buffer-name))
        (with-current-buffer buf
          (unless (eq major-mode 'vterm-mode)
            (vterm-mode)))
        (display-buffer buf '((display-buffer-in-side-window)
                              (side . bottom) (slot . 0)
                              (window-height . 0.25)))
        (select-window (get-buffer-window buf))))))

;; Doom's binding: SPC o t toggles the terminal popup.
(my-leader
 '(("o t" . my/vterm-toggle)))

(provide 'my-term)
;;; vterm.el ends here
