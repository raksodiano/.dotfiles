;;; org.el --- Org-mode, LaTeX and Hugo configuration  -*- lexical-binding: t; -*-

(defvar +my/doom-dir (expand-file-name "~/.config/doom")
  "Doom configuration directory.")

(setq org-directory +my/org-base-dir
      org-roam-directory +my/org-notes-dir
      org-journal-dir (expand-file-name "personal" +my/org-journal-dir)
      org-startup-indented t)

(use-package org
  :custom (org-modules '(org-habit)))

(after! org
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote))

(setq org-clock-sound (expand-file-name "sounds/bell.wav" +my/doom-dir))

(defvar my/sounds-dir (expand-file-name "sounds/" +my/doom-dir)
  "Directory containing notification sound files.")

(defvar my/sound-files
  (list
   (cons 'urgent (expand-file-name "alarm-clock-elapsed.wav" my/sounds-dir))
   (cons 'high (expand-file-name "window-attention.wav" my/sounds-dir))
   (cons 'medium (expand-file-name "dialog-warning.wav" my/sounds-dir))
   (cons 'low (expand-file-name "dialog-information.wav" my/sounds-dir))
   (cons 'normal (expand-file-name "bell.wav" my/sounds-dir))
   (cons 'moderate (expand-file-name "message-new-instant.wav" my/sounds-dir)))
  "Sound files mapped by severity level.")

(defun my/get-org-item-severity ()
  "Get severity from current org item property."
  (let* ((sev-str (org-entry-get (point) "NOTIFY_SEVERITY" t))
         (sev (when sev-str (intern sev-str))))
    sev))

(defun my/play-sound-by-severity (&optional severity)
  "Play notification sound based on SEVERITY level."
  (let* ((sev (or severity 'normal))
         (sound-file (expand-file-name (cdr (assoc sev my/sound-files)) +my/doom-dir))
         (fallback (expand-file-name (cdr (assoc 'normal my/sound-files)) +my/doom-dir)))
    (unless (file-exists-p sound-file)
      (setq sound-file fallback))
    (when (file-exists-p sound-file)
      (start-process-shell-command "play-sound" nil (format "aplay -q %s" sound-file)))))

(defun my/play-notification-sound (severity)
  "Play notification sound with SEVERITY for visual notification."
  (message "Calling play-notification-sound with severity: %s" severity)
  (my/play-sound-by-severity severity))

(use-package! org-alert
  :after org
  :preface
  (setq org-alert-notify-cutoff 1
        org-alert-notify-after-event-cutoff 2)
  :config
  (setq org-alert-notification-title "Org Reminder"
        org-alert-interval 60
        alert-default-style 'libnotify)
  (org-alert-enable))

(defvar my/org-alert-notification-count 0)

(advice-add #'org-alert-check :before
            (lambda (&rest _)
              (setq my/org-alert-notification-count 0)))

(defvar my/alert-called nil)

(advice-add #'alert :around
            (lambda (orig-fun &rest args)
              (setq my/alert-called t)
              (apply orig-fun args)))

(advice-add #'org-alert--dispatch :after
            (lambda (&rest _)
              (when my/alert-called
                (setq my/org-alert-notification-count (1+ my/org-alert-notification-count)
                      my/alert-called nil))))

(advice-add #'org-alert-check :after
            (lambda (&rest _)
              (when (> my/org-alert-notification-count 0)
                (my/play-sound-by-severity 'high))))

;; Configure org alerts

(use-package! org-journal
  :defer t
  :custom
  (org-journal-dir (expand-file-name "personal" +my/org-journal-dir))
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-format "")
  (org-journal-enable-agenda-integration t))

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

(map! :leader
      (:prefix ("n j" . "journal")
       :desc "Open personal journal" "p" #'org-journal-new-entry
       :desc "Open work journal"  "w" #'my/org-journal-open-work))

(defvar +my/org-work-dir (expand-file-name "work" +my/org-notes-dir)
  "Directory for work-related notes.")

(setq org-agenda-files
      (append
       (directory-files-recursively +my/org-agenda-dir "\\.org$")
       (directory-files-recursively +my/org-notes-dir "\\.org$")))

(setq org-agenda-files-work
      (directory-files-recursively +my/org-work-dir "\\.org$"))

(setq org-agenda-custom-commands
      '(("w" "Work Agenda"
         ((agenda "" ((org-agenda-files org-agenda-files-work)))
          (todo "" ((org-agenda-files org-agenda-files-work)))))))

(setq org-edit-src-content-indentation 2)

(setq-default fill-column 120)
(setq visual-fill-column-width 120
      visual-fill-column-center-text t)
(add-hook 'org-mode-hook #'visual-fill-column-mode)

(defface +org-todo-wait
    '((t (:inherit (bold warning org-todo))))
  "Face for WAIT tasks.")
(defface +org-todo-active
    '((t (:inherit (bold org-todo))))
  "Face for NEXT tasks.")
(defface +org-todo-onhold
    '((t (:inherit (italic org-todo))))
  "Face for EVENT tasks.")
(defface +org-todo-project
    '((t (:inherit (bold font-lock-doc-face org-todo))))
  "Face for PROJ tasks.")
(defface +org-todo-cancel
    '((t (:inherit (shadow org-done))))
  "Face for CANCELLED tasks.")

(setq org-todo-keywords
      '((sequence
         "TODO(t)"
         "NEXT(n)"
         "PROJ(p)"
         "WAIT(w)"
         "IDEA(i)"
         "EVENT(e)"
         "|"
         "DONE(d)"
         "CANCELLED(c)")))

(setq org-todo-keyfaces
      '(("TODO"      . +org-todo-active)
        ("NEXT"      . +org-todo-active)
        ("PROJ"      . +org-todo-project)
        ("WAIT"      . +org-todo-wait)
        ("IDEA"      . +org-todo-onhold)
        ("EVENT"     . +org-todo-onhold)
        ("DONE"      . +org-todo-done)
        ("CANCELLED" . +org-todo-cancel)))

(defun my/org-notas-trabajo-file ()
  "Returns a file path based on current date for work notes."
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
     (file+headline ,(my/org-notas-trabajo-file) "Work Notes")
     "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1)

    ("g" "Games")

    ("gg" "Game's Notes" entry
     (file+headline ,(expand-file-name "games/notes.org" +my/org-notes-dir) "Game's Notes")
     "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("gi" "Game ideas" entry
     (file+headline ,(expand-file-name "games/ideas.org" +my/org-notes-dir) "Game ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t))
  "Org capture templates with dynamic paths.")

(setq org-capture-templates +my/org-capture-paths)

(defun my/org-mode-set-language ()
  "Sets the Hunspell dictionary according to the #+LANGUAGE option of the Org buffer."
  (when (derived-mode-p 'org-mode)
    (let ((lang (cdr (assoc "LANGUAGE" (org-collect-keywords '("LANGUAGE"))))))
      (when lang
        (setq-local ispell-dictionary (downcase (car lang)))))))

(add-hook 'org-mode-hook #'my/org-mode-set-language)

(after! ox-latex
  (let* ((class-dir (expand-file-name "latex-classes/" doom-user-dir))
         (themes '(("report-custom" . "report.cls")
                   ("poem" . "poem.cls"))))

    (dolist (theme themes)
      (let* ((name (car theme))
             (file (cdr theme))
             (path (expand-file-name file class-dir)))
        (when (file-exists-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (let ((class-str (buffer-string)))
              (add-to-list 'org-latex-classes
                           (list name class-str
                                 '("\\section{%s}" . "\\section*{%s}")
                                 '("\\subsection{%s}" . "\\subsection*{%s}")
                                 '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                 '("\\paragraph{%s}" . "\\paragraph*{%s}")
                                 '("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                           t)))))

    (let ((styles-dir (expand-file-name "latex-classes/styles/" doom-user-dir)))
      (setenv "TEXINPUTS" (concat styles-dir ":" (getenv "TEXINPUTS"))))

    (setq org-latex-compiler "xelatex"
          org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"
                                  "xelatex -interaction nonstopmode -output-directory %o %f"))))

(after! ox-hugo
  (let ((blog-dir (expand-file-name "~/Workspace/blog")))
    (setq org-hugo-base-dir blog-dir
          org-hugo-content-directory "content-org"
          org-hugo-section "posts"
          org-hugo-preserve-filing 'force
          org-hugo-auto-set-lastmod t
          org-hugo-export-with-toc nil
          org-hugo-allow-spaces-in-tags t
          org-hugo-paired-shortcodes "note,warning,tip,details"
          org-hugo-taxonomy-tags "tags"
          org-hugo-taxonomy-categories "categories"
          org-hugo-static-file-extensions '("png" "jpg" "jpeg" "gif" "svg" "pdf" "css" "js" "woff" "woff2" "ttf")
          org-hugo-languages '(("es" . "Spanish") ("en" . "English"))))))

(setq deft-directory +my/org-base-dir)
(setq deft-recursive t)
(setq deft-extensions '("org" "md" "txt"))
(setq deft-default-extension "org")
(setq deft-ignore-file-regexp "\\.#\\|~$")

(provide '+my-org)
;;; org.el ends here
