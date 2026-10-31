;;; writer.el -*- lexical-binding: t; -*-
;;; writer.el --- Writer mode configuration (split out)

;;; Code:

(use-package olivetti
  :ensure nil
  :commands olivetti-mode
  :custom
  (olivetti-body-width 120))

(use-package lorem-ipsum
  :commands (lorem-ipsum-insert-sentences lorem-ipsum-insert-paragraphs))

(defvar my/writer-previous-window-config nil)

(defun my/writer-focus-section ()
  "Focus current org subtree in a centered writing environment."
  (interactive)
  (setq my/writer-previous-window-config (current-window-configuration))
  (org-tree-to-indirect-buffer)
  (delete-other-windows)
  (olivetti-mode 1))

(defun my/writer-exit-focus ()
  "Restore previous window layout."
  (interactive)
  (when my/writer-previous-window-config
    (set-window-configuration my/writer-previous-window-config)
    (setq my/writer-previous-window-config nil)))

(defun my/writer-split-view ()
  "Split: outline left, focused section right."
  (interactive)
  (delete-other-windows)
  (let ((outline-win (selected-window)))
    (split-window-right)
    (other-window 1)
    (org-tree-to-indirect-buffer)
    (olivetti-mode 1)
    (select-window outline-win)
    (olivetti-mode 1)))

(defun my/writer-mode-toggle ()
  "Toggle full writer environment."
  (interactive)
  (if my/writer-previous-window-config
      (my/writer-exit-focus)
    (my/writer-focus-section)))

(with-eval-after-load 'org
  (my-leader
   '(("o W f" . my/writer-focus-section)
     ("o W s" . my/writer-split-view)
     ("o W w" . my/writer-mode-toggle)
     ("o W q" . my/writer-exit-focus))))

(provide 'my-org-writer)
;;; writer.el ends here
