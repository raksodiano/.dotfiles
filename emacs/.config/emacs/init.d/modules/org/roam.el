;;; roam.el -*- lexical-binding: t; -*-
;;; roam.el --- Org Roam (split out)

;;; Code:

(my/create-directories)

(use-package org-roam
  :custom
  (org-roam-directory +my/org-notes-dir)
  (org-roam-completion-everywhere t)
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode 1))

(my-leader
 '(("n r f" . org-roam-node-find)
   ("n r i" . org-roam-node-insert)
   ("n r c" . org-roam-capture)
   ("n r l" . org-roam-buffer-toggle)))

(provide 'my-org-roam)
;;; roam.el ends here
