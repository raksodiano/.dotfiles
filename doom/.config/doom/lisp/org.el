;;; org.el --- Org-mode, LaTeX and Hugo configuration  -*- lexical-binding: t; -*-

(setq org-directory "~/Org"
      org-roam-directory "~/Org/notes"
      org-journal-dir "~/Org/journal"
      org-startup-indented t)

(use-package org
  :custom (org-modules '(org-habit)))

(after! org
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote))

(setq org-clock-sound "~/.config/doom/sounds/bell.wav")

(defvar my/sounds-dir (expand-file-name "~/.config/doom/sounds/")
  "Directory containing notification sound files.")

(defvar my/sound-files
  (list
   (cons 'urgent "~/.config/doom/sounds/alarm-clock-elapsed.wav")
   (cons 'high "~/.config/doom/sounds/window-attention.wav")
   (cons 'medium "~/.config/doom/sounds/dialog-warning.wav")
   (cons 'low "~/.config/doom/sounds/dialog-information.wav")
   (cons 'normal "~/.config/doom/sounds/bell.wav")
   (cons 'moderate "~/.config/doom/sounds/message-new-instant.wav"))
   "Sound files mapped by severity level.")

(defun my/get-org-item-severity ()
  "Get severity from current org item property."
  (let* ((sev-str (org-entry-get (point) "NOTIFY_SEVERITY" t))
         (sev (when sev-str (intern sev-str))))
    sev))

(defun my/play-sound-by-severity (&optional severity)
  "Play notification sound based on SEVERITY level."
  (let* ((sev (or severity 'normal))
         (sound-file (expand-file-name (cdr (assoc sev my/sound-files)) "~/.config/doom")))
    (unless (file-exists-p sound-file)
      (setq sound-file (expand-file-name (cdr (assoc 'normal my/sound-files)) "~/.config/doom")))
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

;; (setq alert-sound-play-command "aplay -q %s")

;; (setq alert-sound-play-command "aplay -q %s")

(use-package! org-journal
  :defer t
  :custom
  (org-journal-dir "~/Org/journal/personal/")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-time-format "")
  (org-journal-enable-agenda-integration t))

(defun my/org-journal-open-work ()
  "Open or create today's work journal entry."
  (interactive)
  (let ((org-journal-dir "~/Org/journal/work/")
        (org-journal-file-format "%Y-%m-%d.org")
        (org-journal-date-format "%A, %d %B %Y")
        (org-journal-date-prefix "#+TITLE: ")
        (org-journal-enable-agenda-integration nil))
    (org-journal-new-entry t)))

(map! :leader
      (:prefix ("n j" . "journal")
       :desc "Open personal journal" "p" #'org-journal-new-entry
       :desc "Open work journal"  "w" #'my/org-journal-open-work))

(setq org-agenda-files
      (append
       (directory-files-recursively "~/Org/agenda" "\\.org$")
       (directory-files-recursively "~/Org/notes" "\\.org$")))

(setq org-agenda-files-work
      (directory-files-recursively "~/Org/notes/work" "\\.org$"))

(setq org-agenda-custom-commands
      '(("w" "Work Agenda"
         ((agenda "" ((org-agenda-files (directory-files-recursively "~/Org/notes/work" "\\.org$"))))
          (todo "" ((org-agenda-files (directory-files-recursively "~/Org/notes/work" "\\.org$"))))))))

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
         (ruta (expand-file-name fecha "~/Org/notes/work/")))
    (make-directory (file-name-directory ruta) t)
    ruta))

(setq org-capture-templates
  `(("a" "Events" entry
     (file+headline "~/Org/agenda/agenda.org" "Events")
     "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:"
     :empty-lines 1 :mkdir t)

    ("t" "General Task" entry
     (file+headline "~/Org/notes/taks.org" "General Task")
     "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("p" "Personal Notes" entry
     (file+headline "~/Org/notes/personal/notes.org" "Personal Notes")
     "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("b" "Book Ideas" entry
     (file+headline "~/Org/notes/book/ideas.org" "Book Ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("h" "Blog Ideas" entry
     (file+headline "~/Org/notes/blog/posts.org" "Blog Ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("w" "Work Notes" entry
     (file+headline ,(my/org-notas-trabajo-file) "Work Notes")
     "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1)

    ("g" "Games")

    ("gg" "Game's Notes" entry
     (file+headline "~/Org/notes/games/notes.org" "Game's Notes")
     "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)

    ("gi" "Game ideas" entry
     (file+headline "~/Org/notes/games/ideas.org" "Game ideas")
     "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
     :empty-lines 1 :mkdir t)))

(defun my/org-mode-set-language ()
  "Sets the Hunspell dictionary according to the #+LANGUAGE option of the Org buffer."
  (when (derived-mode-p 'org-mode)
    (let ((lang (cdr (assoc "LANGUAGE" (org-collect-keywords '("LANGUAGE"))))))
      (when lang
        (setq-local ispell-dictionary (downcase (car lang)))))))

(add-hook 'org-mode-hook #'my/org-mode-set-language)

(after! ox-latex
  (let* ((class-dir (expand-file-name "latex-classes/" doom-user-dir))
         (themes
          '(("report-custom" . "report.cls")
            ("poem" . "poem.cls"))))

    (dolist (theme themes)
      (let* ((name  (car theme))
             (file  (cdr theme))
             (path  (expand-file-name file class-dir)))
        (when (file-exists-p path)
          (let ((class-str (with-temp-buffer
                             (insert-file-contents path)
                             (buffer-string))))
            (add-to-list 'org-latex-classes
                         `(,name
                           ,class-str
                           ("\\section{%s}"       . "\\section*{%s}")
                           ("\\subsection{%s}"    . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
                         t))))))

  (let ((styles-dir (expand-file-name "latex-classes/styles/" doom-user-dir)))
    (setenv "TEXINPUTS"
            (concat styles-dir ":" (getenv "TEXINPUTS"))))

  (setq org-latex-compiler "xelatex"
        org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f")))

(after! ox-hugo
  (setq org-hugo-base-dir "~/Workspace/blog"
        org-hugo-content-directory "content-org"
        org-hugo-section "posts"
        org-hugo-preserve-filing 'force
        org-hugo-auto-set-lastmod t
        org-hugo-export-with-toc nil
        org-hugo-allow-spaces-in-tags t
        org-hugo-paired-shortcodes "note,warning,tip,details"

        org-hugo-taxonomy-tags "tags"
        org-hugo-taxonomy-categories "categories"

        org-hugo-static-file-extensions
        '("png" "jpg" "jpeg" "gif" "svg" "pdf" "css" "js" "woff" "woff2" "ttf")

        org-hugo-languages '(("es" . "Spanish")
                             ("en" . "English"))))

(setq deft-directory "~/Org/")
(setq deft-recursive t)
(setq deft-extensions '("org" "md" "txt"))
(setq deft-default-extension "org")
(setq deft-ignore-file-regexp "\\.#\\|~$")

(provide '+my-org)
;;; org.el ends here
