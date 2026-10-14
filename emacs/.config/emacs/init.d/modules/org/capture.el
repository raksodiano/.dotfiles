;;; capture.el -*- lexical-binding: t; -*-
;;; capture.el --- Org capture templates (split out of Doom's org.el)

;;; Code:

(defun my/org-work-notes-file ()
  "Return dated file path for work notes."
  (let* ((fecha (format-time-string "%Y/%m/%d-note.org"))
         (ruta (expand-file-name fecha +my/org-work-dir)))
    (make-directory (file-name-directory ruta) t)
    ruta))

(defvar +my/org-capture-paths
  `(("a" "Events" entry
     (file+headline ,(expand-file-name "agenda.org" +my/org-agenda-dir) "Events")
     "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:"
     :empty-lines 1 :mkdir t)

    ("t" "General Task" entry
     (file+headline ,(expand-file-name "tasks.org" +my/org-notes-dir) "General Task")
     "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("p" "Personal Notes" entry
     (file+headline ,(expand-file-name "personal/notes.org" +my/org-notes-dir) "Personal Notes")
     "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("b" "Book Ideas" entry
     (file+headline ,(expand-file-name "book/ideas.org" +my/org-notes-dir) "Book Ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("h" "Blog Ideas" entry
     (file+headline ,(expand-file-name "blog/posts.org" +my/org-notes-dir) "Blog Ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("w" "Work Notes" entry
     (file+headline ,(my/org-work-notes-file) "Work Notes")
     "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1)

    ("g" "Games")

    ("gg" "Game's Notes" entry
     (file+headline ,(expand-file-name "games/notes.org" +my/org-notes-dir) "Game's Notes")
     "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("gi" "Game Ideas" entry
     (file+headline ,(expand-file-name "games/ideas.org" +my/org-notes-dir) "Game Ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t))
  "Org capture templates with dynamic paths.")

(setq org-capture-templates +my/org-capture-paths)

;; Bind capture to SPC n? Use my-leader.
(my-leader
 '(("j c" . org-capture)))

(provide 'my-org-capture)
;;; capture.el ends here
