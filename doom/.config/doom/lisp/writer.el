;;; writer.el --- Writer mode configuration  -*- lexical-binding: t; -*-

(use-package! olivetti
  :commands olivetti-mode
  :init
  (setq olivetti-body-width 120))

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

(after! org
    (map! :map org-mode-map
     :localleader
     (:prefix ("W" . "writer")
      :desc "Focus section"     "f" #'my/writer-focus-section
      :desc "Split view"        "s" #'my/writer-split-view
      :desc "Toggle focus"      "w" #'my/writer-mode-toggle
      :desc "Exit focus"        "q" #'my/writer-exit-focus)))

(provide '+my-writer)
;;; writer.el ends here
