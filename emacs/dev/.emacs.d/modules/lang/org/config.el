;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf org-beautify-theme
      :straight t
      :disabled
      :after org)

(leaf org-bullets
  :straight t
  :after org
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(leaf org-download
  :straight t
  :after org
  :init
  (setq-default org-download-image-dir org-imagenes-dir))

(leaf ob-restclient
  :straight t
  :after org)

(leaf htmlize
  :straight t
  :after org)

(leaf deft
  :straight t
  :after org)

(leaf noflet
  :straight t)

(leaf ob-translate
      :straight t)

(leaf org
      :straight t
      :straight org-plus-contrib
      ;; :pin org
      :bind (("C-c a" . org-agenda-list)
             ("C-c c" . org-capture)
             ("C-c C-w" . org-refile))
      :config
      (progn
        (add-hook 'org-mode-hook
                  (lambda ()
                    (turn-on-auto-fill)
                    (switch-dictionary)))

        (setq org-clock-persist-file (concat cache-dir "org-clock-save.el")
              org-clock-persist 'history
              org-src-fontify-natively t
              org-enforce-todo-dependencies t
              org-html-validation-link nil)

        (org-clock-persistence-insinuate)

        (add-hook 'org-mode-hook (lambda ()
                                   (org-indent-mode t)
                                   (diminish 'org-indent-mode)))

        (org-babel-do-load-languages 'org-babel-load-languages
                                     '((python . t)
                                       (shell . t)
                                       (lisp . t)
                                       (sql . t)
                                       (restclient . t)
                                       (dot . t)
                                       (plantuml . t)
                                       (emacs-lisp . t)))

        ;; set the modules enabled by default
        (setq org-modules
              '(org-bbdb org-bibtex org-bullets toc-org org-download ob-restclient
                         org-tree-slide org-docview org-mhe org-rmail org-crypt org-protocol
                         org-pomodoro org-gnus org-id org-info org-habit org-irc org-annotate-file
                         org-eval org-expiry org-man org-panel org-toc ox-md ox-latex))

        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

      ;; set default ellipsis
      ;; (setq org-bullets-bullet-list '("☣" "✡" "✽" "✲" "✱" "✻" "✽" "✾" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
      (setq org-bullets-bullet-list '("⬢" "◆" "▲" "■"))
      (setq org-ellipsis " ⤵")

      ;; set default directories
      (setq org-id-locations-file (concat cache-dir "org-id.el")
            org-directory org-dir
            org-default-notes-file (concat org-directory "notes.org"))

      ;; set the archive
      (setq org-archive-location (concat org-directory "archive.org::datetree/** Archived"))

      ;; highlight code blocks syntax
      (setq org-src-fontify-natively  t
            org-src-tab-acts-natively t)

      ;; more sane emphasis regex to export to HTML as substitute of Markdown
      (org-set-emph-re 'org-emphasis-regexp-components
                       '(" \t({"
                         "- \t.,:!?;)}[:multibyte:]"
                         " \t\r\n,"
                         "."
                         1))

      ;; tasks management
      (setq org-refile-targets '((org-agenda-files :maxlevel . 5))
            org-log-done t
            org-clock-idle-time nil
            org-todo-keywords (quote((sequence "TODO(t)" "IN-PROGRESS(i)" "NEXT(n)" "|" "DONE(d)")
                                     (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)" "PHONE(p)" "MEETING(m)"))))

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-allow-creating-parent-nodes 'confirm)

      ;; agenda & diary
      (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
            org-agenda-include-diary t
            org-agenda-tags-todo-honor-ignore-options t
            org-agenda-start-on-weekday 0
            org-agenda-start-day "-1d"
            org-agenda-timegrid-use-ampm 1
            org-agenda-inhibit-startup t
            org-agenda-files (list org-directory)
            org-agenda-window-setup (quote current-window)
            org-deadline-warning-days 7
            org-agenda-span (quote fortnight)
            org-agenda-skip-scheduled-if-deadline-is-shown t
            org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
            org-agenda-todo-ignore-deadlines (quote all)
            org-agenda-todo-ignore-scheduled (quote all)
            org-agenda-sorting-strategy (quote
                                         ((agenda deadline-up priority-down)
                                          (todo priority-down category-keep)
                                          (tags priority-down category-keep)
                                          (search category-keep))))

      (eval-after-load 'eww
        '(progn (org-link-set-parameters "eww" :follow #'eww :store #'org-eww-store-link)))

      ;; date insertion configuration
      (setq org-expiry-created-property-name "CREATED"
            org-expiry-inactive-timestamps t
            org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                                 ("WAITING" ("WAITING" . t))
                                                 ("HOLD" ("WAITING") ("HOLD" . t))
                                                 (done ("WAITING") ("HOLD"))
                                                 ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                                 ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                                 ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

      ;; capture
      (setq org-reverse-note-order t)
      (setq org-capture-templates '(("t" "Todo [Work]"
                                     entry
                                     (file "~/org/work.org")
                                     "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n  %i\n")
                                    ("d" "Todo w/date [Work]"
                                     entry
                                     (file "~/org/work.org")
                                     "* TODO %? %<%Y-%m-%d>\n:PROPERTIES:\n:CREATED: %U\n:END:\n  %i\n")
                                    ("l" "Link currently stored [Work]"
                                     entry
                                     (file "~/org/work.org")
                                     "* TODO %i%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\%A\n%i\n")
                                    ("m" "Meeting [Work]"
                                     entry
                                     (file "~/org/work.org")
                                     "* Meeting %<%Y-%m-%d>: %^{prompt}\n:PROPERTIES:\n:CREATED: %U\n:END:\n- [ ] %?\n\n")))

      ;; (setq org-capture-templates '(("w" "Work TODO"
      ;;                                entry
      ;;                                (file "~/org/work.org" "Work" "Tasks")
      ;;                                "* TODO %? \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:PROPERTIES:\n:CATEGORY: TASKS\n:CREATED: %U\n:END:")
      ;;                               ("o" "Work Overtime"
      ;;                                entry
      ;;                                (file (concat org-dir "work.org") "Work" "COMMENT Overtime")
      ;;                                "* %? \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:PROPERTIES:\n:CREATED: %U\n:END:")
      ;;                               ("m" "Work Meetings"
      ;;                                entry
      ;;                                (file (concat org-dir "work.org") "Work" "Meetings")
      ;;                                "* %? \nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:PROPERTIES:\n:CATEGORY: MEETINGS\n:CREATED: %U\n:END:")
      ;;                               ("t" "Work Training's"
      ;;                                entry
      ;;                                (file (concat org-dir "work.org") "Work" "Training's")
      ;;                                "* %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:PROPERTIES:\n:CATEGORY: TRAINING'S\n:CREATED: %U\n:END:")
      ;;                               ("S" "Stuff TODO"
      ;;                                entry
      ;;                                (file (concat org-dir "stuff.org") "Stuff" "Tasks")
      ;;                                "* TODO %? \n:PROPERTIES:\n:CATEGORY: TASKS\n:CREATED: %U\n:END:")
      ;;                               ("M" "Stuff Meetings"
      ;;                                entry
      ;;                                (file  (concat org-dir "stuff.org") "Stuff" "Meetings")
      ;;                                "* %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:PROPERTIES:\n:CATEGORY: MEETINGS\n:CREATED: %U\n:END:")
      ;;                               ("T" "Stuff Training's"
      ;;                                entry
      ;;                                (file (concat org-dir "stuff.org") "Stuff" "Training's")
      ;;                                "* %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n:PROPERTIES:\n:CATEGORY: TRAINING'S\n:CREATED: %U\n:END:")))

      ;; configure the external apps to open files
      (add-to-list (quote org-file-apps)
                   (quote ("\\.pdf\\'" lambda (file link) (org-pdfview-open link))))

      ;; protect hidden trees for being inadvertily edited (do not work with evil)
      (setq-default org-catch-invisible-edits  'error
                    org-ctrl-k-protect-subtree 'error)

      ;; show images inline
      ;; only works in GUI, but is a nice feature to have
      (when (window-system)
        (setq org-startup-with-inline-images t))
      ;; limit images width
      (setq org-image-actual-width '(800))

      ;; :::::: Org-Babel ::::::

      ;; languages supported
      (org-babel-do-load-languages
       (quote org-babel-load-languages)
       (quote ((calc . t)
               (dot . t)
               (emacs-lisp . t)
               (latex . t)
               (org . t)
               (makefile . t)
               (plantuml . t)
               (python . t)
               (ruby . t)
               (shell . t)
               (sqlite . t)
               (sql . t))))
      (setq org-babel-python-command "python3")

      ;; refresh images after execution
      (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

      ;; don't ask confirmation to execute "safe" languages
      (defun org-confirm-babel-evaluate (lang body)
        (and (not (string= lang "ditaa"))
             (not (string= lang "dot"))
             (not (string= lang "gnuplot"))
             (not (string= lang "ledger"))
             (not (string= lang "plantuml"))))
      (setq org-confirm-babel-evaluate 'org-confirm-babel-evaluate))

;; Disable flycheck when edit org-src-block
(defun disable-fylcheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'disable-fylcheck-in-org-src-block)

(setq deft-directory org-dir)
(setq deft-extension '("txt" "tex" "org"))
(setq deft-text-mode 'org-mode)
(setq deft-use-filename-as-title t)
(setq deft-auto-save-interval 0)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(defun org-insert-file (filename)
  "Insert Elisp code block recreating file named FILENAME."
  (interactive "f")
  (let ((base64-string
         (with-temp-buffer
           (insert-file-contents-literally filename)
           (base64-encode-region (point-min) (point-max))
           (buffer-string))))
    (insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))

(leaf ob-python
      :leaf-defer t
      :straight org-plus-contrib
      ;; :ensure org-plus-contrib
      :commands (org-babel-execute:python))

(leaf ob-shell
      :leaf-defer t
      :straight org-plus-contrib
      ;; :ensure org-plus-contrib
      :commands (org-babel-execute:sh
                 org-babel-expand-body:sh
                 org-babel-execute:bash
                 org-babel-expand-body:bash))

(leaf ob-plantuml
      :leaf-defer t
      :straight org-plus-contrib
      ;; :ensure org-plus-contrib
      :commands (org-babel-execute:plantuml))

(leaf ob-async
      :straight t)

(leaf org-super-agenda
      :straight t
      :after org)
      ;; :init (org-super-agenda-mode)
      ;; (setq org-deadline-warning-days 7)
      ;; (setq org-deadline-warning-days 7)
      ;; (setq org-agenda-skip-scheduled-if-done t)
      ;; (setq org-agenda-start-on-weekday nil)
      ;; :config
      ;; (let ((org-super-agenda-groups
      ;;        '(;; Each group has an implicit boolean OR operator between its selectors.
      ;;          (:name "Today"  ; Optionally specify section name
      ;;                 :time-grid t  ; Items that appear on the time grid
      ;;                 :todo "TODAY")  ; Items that have this TODO keyword
      ;;          (:name "Important"
      ;;                 ;; Single arguments given alone
      ;;                 :tag "bills"
      ;;                 :priority "A")
      ;;          ;; Set order of multiple groups at once
      ;;          (:order-multi (2 (:name "Shopping in town"
      ;;                                  ;; Boolean AND group matches items that match all subgroups
      ;;                                  :and (:tag "shopping" :tag "@town"))
      ;;                           (:name "Food-related"
      ;;                                  ;; Multiple args given in list with implicit OR
      ;;                                  :tag ("food" "dinner"))
      ;;                           (:name "Personal"
      ;;                                  :habit t
      ;;                                  :tag "personal")
      ;;                           (:name "Space-related (non-moon-or-planet-related)"
      ;;                                  ;; Regexps match case-insensitively on the entire entry
      ;;                                  :and (:regexp ("space" "NASA")
      ;;                                                ;; Boolean NOT also has implicit OR between selectors
      ;;                                                :not (:regexp "moon" :tag "planet")))))
      ;;          ;; Groups supply their own section names when none are given
      ;;          (:todo "WAITING" :order 8)  ; Set order of this section
      ;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
      ;;                 ;; Show this group at the end of the agenda (since it has the
      ;;                 ;; highest number). If you specified this group last, items
      ;;                 ;; with these todo keywords that e.g. have priority A would be
      ;;                 ;; displayed in that group instead, because items are grouped
      ;;                 ;; out in the order the groups are listed.
      ;;                 :order 9)
      ;;          (:priority<= "B"
      ;;                       ;; Show this section after "Today" and "Important", because
      ;;                       ;; their order is unspecified, defaulting to 0. Sections
      ;;                       ;; are displayed lowest-number-first.
      ;;                       :order 1)
      ;;          ;; After the last group, the agenda will display items that didn't
      ;;          ;; match any of these groups, with the default order position of 99
      ;;          )))
      ;; (org-agenda nil "a"))

(leaf org-tree-slide
      :straight t)

;; (leaf org-tree-slide
;;   :straight t
;;   :config
;;   (progn
;;     (setq org-tree-slide--lighter " Slide")

;;     (defvar org-tree-slide-text-scale 4
;;       "Text scale ratio to default when `org-tree-slide-mode' is enabled.")

;;     (defun org-tree-slide-set-profile ()
;;       "Customize org-tree-slide variables."
;;       (interactive)
;;       (setq org-tree-slide-header t)
;;       (setq org-tree-slide-slide-in-effect nil)
;;       (setq org-tree-slide-heading-emphasis t)
;;       (setq org-tree-slide-cursor-init t) ;Move cursor to the head of buffer
;;       (setq org-tree-slide-modeline-display 'lighter)
;;       (setq org-tree-slide-skip-done nil)
;;       (setq org-tree-slide-skip-comments t)
;;       (setq org-tree-slide-activate-message
;;             (concat "Starting Org presentation. "
;;                     "Use arrow keys to navigate the slides."))
;;       (setq org-tree-slide-deactivate-message "Ended presentation.")
;;       (message "Custom `org-tree-slide' profile: ON"))

;;     (defvar writegood-mode-state nil
;;       "Variable to store the state of `writegood-mode'.")

;;     (defun org-tree-slide-start ()
;;       "Set up the frame for the slideshow."
;;       (interactive)
;;       (when (fboundp 'writegood-mode)
;;         (setq writegood-mode-state writegood-mode)
;;         (writegood-mode -1))
;;       (flyspell-mode -1)
;;       (text-scale-set org-tree-slide-text-scale))
;;     (add-hook 'org-tree-slide-play-hook #'org-tree-slide-start)

;;     (defun org-tree-slide-stop()
;;       "Undo the frame setup for the slideshow."
;;       (interactive)
;;       (when (and (fboundp 'writegood-mode)
;;                  writegood-mode-state)
;;         (writegood-mode 1)
;;         (setq writegood-mode-state nil))
;;       (flyspell-mode 1)
;;       (text-scale-set 0))
;;     (add-hook 'org-tree-slide-stop-hook #'org-tree-slide-stop)

;;     (defun org-tree-slide-text-scale-reset ()
;;       "Reset time scale to `modi/org-tree-slide-text-scale'."
;;       (interactive)
;;       (text-scale-set org-tree-slide-text-scale))

;;     (defun org-tree-slide-text-scale-inc1 ()
;;       "Increase text scale by 1."
;;       (interactive)
;;       (text-scale-increase 1))

;;     (defun org-tree-slide-text-scale-dec1 ()
;;       "Decrease text scale by 1."
;;       (interactive)
;;       (text-scale-decrease 1))

;;     (bind-keys
;;      :map org-tree-slide-mode-map
;;      ("C-b" . org-tree-slide-move-previous-tree)
;;      ("C-f" . org-tree-slide-move-next-tree)
;;      ("C-0" . org-tree-slide-text-scale-reset)
;;      ("C-+" . org-tree-slide-text-scale-inc1)
;;      ("C--" . org-tree-slide-text-scale-dec1)
;;      ("C-1" . org-tree-slide-content)
;;      ("C-2" . org-tree-slide-set-profile)
;;      ("C-3" . org-tree-slide-simple-profile)
;;      ("C-4" . org-tree-slide-presentation-profile))))

(provide 'config)
;;; config.el ends here
