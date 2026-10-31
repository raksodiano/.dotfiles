;;; journal.el -*- lexical-binding: t; -*-
;;; journal.el --- Org journal (split out)

;;; Code:

(my/create-directories)

(use-package org-journal
  :custom
  (org-journal-dir +my/org-journal-dir)
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-format "")
  (org-journal-enable-agenda-integration t))

;; `org-journal--cache-file' is a defconst computed from
;; `user-emacs-directory' when the package loads, so it cannot be set with
;; `:custom'. Override it afterwards to keep the cache out of the repo.
(with-eval-after-load 'org-journal
  (with-no-warnings
    (setq org-journal--cache-file
          (expand-file-name "org-journal.cache" my/emacs-cache))))

(defvar +my/org-journal-work-dir (expand-file-name "work" +my/org-journal-dir)
  "Directory for work journal entries.")

(defun my/org-journal-open-work ()
  "Open or create today's work journal entry."
  (interactive)
  (let ((org-journal-dir +my/org-journal-work-dir)
        (org-journal-file-format "%Y-%m-%d.org")
        (org-journal-date-format "%A, %d %B %Y")
        (org-journal-date-prefix "#+TITLE: ")
        (org-journal-enable-agenda-integration nil))
    (org-journal-new-entry t)))

(my-leader
 '(("n j p" . org-journal-new-entry)
   ("n j w" . my/org-journal-open-work)))

(provide 'my-org-journal)
;;; journal.el ends here
