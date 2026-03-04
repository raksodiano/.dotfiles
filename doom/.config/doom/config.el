;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Visual configuration and performance
;; -------------------------------

;; Line wrapping
(global-visual-line-mode t)

;; Send files to trash instead of fully deleting
(setq delete-by-moving-to-trash t)

;; Save automatically
(setq auto-save-default t)

;; Performance optimizations
(setq gc-cons-threshold (* 256 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq comp-deferred-compilation t)
(setq comp-async-jobs-number 8)

;; Garbage collector optimization
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 1024 1024 1024))

;; Version control optimization
(setq vc-handled-backends '(Git))

;; Fix x11 issues
(setq x-no-window-manager t)
(setq frame-inhibit-implied-resize t)
(setq focus-follows-mouse nil)

(setq doom-load-packages-incrementally nil)
(remove-hook 'doom-after-init-hook #'doom-load-packages-incrementally-h)

;; Start emacs maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Setup custom splashscreen
(setq fancy-splash-image "~/.config/doom/images/gnu_color.png")

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Welcome Home, Master.")))

;; Blink cursor
(blink-cursor-mode 1)

;; Theme
(setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-gruvbox)

;; Maintain terminal transparency in Doom Emacs
(after! doom-themes
  (unless (display-graphic-p)
    (set-face-background 'default "undefined")))

;; remove top frame bar in emacs
(add-to-list 'default-frame-alist '(undecorated . t))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(94 . 97))
(add-to-list 'default-frame-alist '(alpha . (94 . 97)))

;; Evil-escape sequence
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.1)

;; Don't move cursor back when exiting insert mode
(setq evil-move-cursor-back nil)
;; granular undo with evil mode
(setq evil-want-fine-undo t)

(setq which-key-idle-delay 0.2)

;; Relative line numbers
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Line model and fonts
(setq doom-font (font-spec :family "GeistMono Nerd Font" :size 15)
      doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
      doom-big-font (font-spec :family "GeistMono Nerd Font" :size 22))

(custom-set-faces!
    '(mode-line :height 90 :inherit 'variable-pitch)
    '(mode-line-inactive :height 80 :inherit 'variable-pitch))

(after! all-the-icons
    (setq all-the-icons-scale-factor 1.5))

;; IDE stile tabs (Centaur Tabs)
(setq centaur-tabs-style "alternate"
      centaur-tabs-height 32
      centaur-tabs-set-bar 'under)

(centaur-tabs-mode t)

;; Folding for languages without native support
(add-hook 'sh-mode-hook #'outline-minor-mode)
(add-hook 'markdown-mode-hook #'outline-minor-mode)
(setq hs-isearch-open t)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(after! highlight-indent-guides
    (setq highlight-indent-guides-method (if (display-graphic-p)
                                             'bitmap
                                           'character)
     highlight-indent-guides-responsive 'top))

(add-transient-hook! 'doom-first-input-hook
  (let ((cell (assoc 'side
                     (assoc "^\\*\\(?:Wo\\)?Man " display-buffer-alist))))
    (setcdr cell 'right)))

(setq require-final-newline 'nil)

(after! persp-mode
    (defadvice! dan/persp-autosave--add-breakline (&rest _)
      "Automatically add breakline for certain buffers before saving to file."
      :before #'basic-save-buffer
      (when (and
             (/= (point-max) (point-min))
             (/= (char-after (1- (point-max))) ?\n)
             (string-equal (file-name-directory
                            (or (buffer-file-name (current-buffer)) ""))
                           persp-save-dir))
        (goto-char (point-max))
        (insert ?\n))))

;; Improving the manual
(add-transient-hook! 'doom-first-input-hook
  (let ((cell (assoc 'side
                     (assoc "^\\*\\(?:Wo\\)?Man " display-buffer-alist))))
    (setcdr cell 'right)))

(use-package! dotenv-mode
  :mode ("\\.env\\.?.*\\'" . dotenv-mode))

;; Align comments
(setq comment-tabs t)

;; Indentation
(setq-default tab-width 2)

;; -------------------------------
;; Modeline configuration
;; -------------------------------

(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)

;; Show icons
(setq doom-modeline-icon t)

;; Show project and file name
;; (setq doom-modeline-project-detection 'auto)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; Show position in buffer
(setq doom-modeline-percent-position t)

;; Show lsp and checker environment if active
(setq doom-modeline-lsp t)

;; Show workspace
(setq doom-modeline-workspace-name t)

;; -------------------------------
;; Dictionaries configuration
;; -------------------------------

;; Function to change Flyspell dictionary
(defun my/change-ispell-dictionary (lang)
  "Change Flyspell dictionary to LANG."
  (interactive "sLanguage (e.g., es_ES or en_US): ")
  (setq ispell-dictionary lang)
  (ispell-change-dictionary lang)
  (message "Dictionary changed to %s" lang))

;; Add keyboard shortcut to easily change dictionary
(map! :leader
      :desc "Change Flyspell dictionary" "l d" #'my/change-ispell-dictionary)

;; -------------------------------
;; Directories configuration
;; -------------------------------

(setq my/directories '("~/Org"
                       "~/Org/rss"
                       "~/Org/journal"
                       "~/Org/journal/personal"
                       "~/Org/journal/work"
                       "~/Org/agenda"
                       "~/Org/notes"
                       "~/Org/notes/blog"
                       "~/Org/notes/book"
                       "~/Org/notes/personal"
                       "~/Org/notes/work"
                       "~/Org/notes/games"
                       "~/Library"
                       "~/Workspace"
                       "~/Workspace/Books"
                       "~/Workspace/Books/latex"
                       "~/Workspace/Books/org"
                       "~/Workspace/Blog"
                       "~/Workspace/Personal"
                       "~/Workspace/Scripts"
                       "~/Workspace/Work"
                       "~/Workspace/Games"))

(dolist (directory my/directories)
  (unless (file-directory-p directory)
    (make-directory directory t)))

;; -------------------------------
;; Lisp configuration
;; -------------------------------

(setq inferior-lisp-program "sbcl")

(after! sly
    (setq sly-lisp-implementations
     '((sbcl ("sbcl")))))

(after! elisp-mode
    (setq-hook! 'emacs-lisp-mode-hook
      indent-tabs-mode nil
      lisp-indent-function #'common-lisp-indent-function))

(setq lisp-indent-function 'common-lisp-indent-function)

;; -------------------------------
;; Writing configuration
;; -------------------------------

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-list))

(map! :leader
      :prefix ("i l" . "lorem ipsum")
      :desc "Insert paragraphs"  "p" #'lorem-ipsum-insert-paragraphs
      :desc "Insert sentences"  "s" #'lorem-ipsum-insert-sentences
      :desc "Insert list items" "l" #'lorem-ipsum-insert-list)

(after! flyspell
    (setq flyspell-lazy-idle-seconds 0.3))

;; -------------------------------
;; Zen Mode configuration
;; -------------------------------

;; Setup writeroom width and appearance
(after! writeroom-mode
  ;; Set width for centered text
  (setq writeroom-width 40)

  ;; Ensure the text is truly centered horizontally
  (setq writeroom-fringes-outside-margins nil)
  (setq writeroom-center-text t)

  ;; Add vertical spacing for better readability
  (setq writeroom-extra-line-spacing 4)  ;; Adds space between lines

  ;; Improve vertical centering with visual-fill-column integration
  (add-hook! 'writeroom-mode-hook
    (defun my-writeroom-settings ()
      "Configure various settings when entering/exiting writeroom-mode."
      (if writeroom-mode
          (progn
            ;; When entering writeroom mode
            (display-line-numbers-mode -1)       ;; Turn off line numbers
            (setq cursor-type 'bar)              ;; Change cursor to a thin bar for writing
            (hl-line-mode -1)                    ;; Disable current line highlighting
            (setq left-margin-width 0)           ;; Let writeroom handle margins
            (setq right-margin-width 0)
            (text-scale-set 1)                   ;; Slightly increase text size

            ;; Improve vertical centering
            (when (bound-and-true-p visual-fill-column-mode)
              (visual-fill-column-mode -1))      ;; Temporarily disable if active
            (setq visual-fill-column-width 40)   ;; Match writeroom width
            (setq visual-fill-column-center-text t)
            (setq visual-fill-column-extra-text-width '(0 . 0))

            ;; Set top/bottom margins to improve vertical centering
            ;; These larger margins push content toward vertical center
            (setq-local writeroom-top-margin-size
                        (max 10 (/ (- (window-height) 40) 3)))
            (setq-local writeroom-bottom-margin-size
                        (max 10 (/ (- (window-height) 40) 3)))

            ;; Enable visual-fill-column for better text placement
            (visual-fill-column-mode 1))

        ;; When exiting writeroom mode
        (progn
          (display-line-numbers-mode +1)       ;; Restore line numbers
          (setq cursor-type 'box)              ;; Restore default cursor
          (hl-line-mode +1)                    ;; Restore line highlighting
          (text-scale-set 0)                   ;; Restore normal text size
          (when (bound-and-true-p visual-fill-column-mode)
            (visual-fill-column-mode -1))))))  ;; Disable visual fill column mode

  ;; Hide modeline for a cleaner look
  (setq writeroom-mode-line nil)

  ;; Add additional global effects for writeroom
  (setq writeroom-global-effects
        '(writeroom-set-fullscreen        ;; Enables fullscreen
          writeroom-set-alpha             ;; Adjusts frame transparency
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width))

  ;; Set frame transparency
  (setq writeroom-alpha 0.95))

;; -------------------------------
;; IA configuration
;; -------------------------------

(use-package! ellama
  :defer t
  :bind ("C-c e" . ellama-chat)
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-language "Spanish")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "qwen2.5:3b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "qwen2.5:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-providers
          '(("text" . (make-llm-ollama
                       :chat-model "qwen2.5:3b"
                       :embedding-model "nomic-embed-text"))
            ("code" . (make-llm-ollama
                       :chat-model "qwen2.5-coder:1.5b"
                       :embedding-model "nomic-embed-text"))
            ("small-text" . (make-llm-ollama
                             :chat-model "gemma2:2b"
                             :embedding-model "nomic-embed-text"))
            ("small-code" . (make-llm-ollama
                             :chat-model "phi3:mini"
                             :embedding-model "nomic-embed-text"))))
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "qwen2.5:0.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "qwen2.5:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-extraction-provider
          (make-llm-ollama
           :chat-model "qwen2.5-coder:1.5b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 8192))))
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  (ellama-context-header-line-global-mode +1)
  (ellama-session-header-line-global-mode +1)
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

;; -------------------------------
;; LSP configuration for all languages
;; -------------------------------

;; LSP Performance optimizations and settings
(after! lsp-mode
  (setq lsp-idle-delay 0.1
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-links nil

        ;; Register custom gopls settings
        lsp-gopls-completeUnimported t
        lsp-gopls-staticcheck t
        lsp-gopls-analyses '((unusedparams . t)
                             (unusedwrite . t))))

;; LSP UI settings for better performance
(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-enable nil
        lsp-ui-peek-enable t))

;; LSP general
(setq lsp-ui-sideline-enable t                ; Information in sidebar
      lsp-headerline-breadcrumb-enable t      ; File path in headerline
      company-minimum-prefix-length 1
      company-idle-delay 0.1)

;; Enable auto-completion and code actions
(setq lsp-enable-suggest-server-download t  ; Download LSP if not installed
      lsp-auto-execute-action t             ; Execute actions automatically (optional)
      lsp-completion-enable t)               ; Integrate with company-mode
                                        ;
;; Python (pyright)
(setq python-shell-interpreter "python3"
      lsp-pyright-multi-root t)

;; JavaScript/TypeScript (tide + lsp)
(use-package! tide
  :hook ((js-mode . tide-setup)
         (typescript-mode . tide-setup)))

;; C/C++ (clangd)
(setq lsp-clients-clangd-executable "/usr/bin/clangd")
;; C/C++ (clang-format)
(setq clang-format-style "Google")  ; Estilo: Google, LLVM, WebKit, etc.

;; Shell Script
(setq lsp-bash-language-server-path "~/.volta/bin/bash-language-server")

;; Use virtual environment for Python
(setq lsp-pyright-venv-path "~/.emacs-lsp-venv")
(setq python-shell-interpreter "~/.emacs-lsp-venv/bin/python3")

;; Python (Black)
(setq format-all-formatters '(("Python" black)))

;; JavaScript/TypeScript (Prettier)
(setq prettier-js-args '("--trailing-comma" "all"
                         "--single-quote" "true"))

;; Shell (shfmt)
(setq shfmt-arguments "-i=2"  ; 2-space indentation
      shfmt-path "~/.go/bin/shfmt")

;; Lisp (standard Emacs indentation)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)  ; Documentation support

;; Enable auto-format on save
(setq +format-on-save-enabled-modes
      '(python-mode js-mode typescript-mode c-mode c++-mode sh-mode lua-mode))

(defun install-lsp-servers ()
  "Install all required LSP servers"
  (interactive)

  ;; Python LSP
  (unless (file-exists-p "~/.emacs-lsp-venv/bin/pylsp")
    (shell-command "python3 -m venv ~/.emacs-lsp-venv")
    (shell-command "~/.emacs-lsp-venv/bin/pip install python-lsp-server pyright pylint black"))

  ;; JavaScript/TypeScript
  (unless (executable-find "typescript-language-server")
    (shell-command "npm install -g typescript-language-server vscode-langservers-extracted typescript"))

  ;; Astro LSP requires TypeScript
  (unless (executable-find "tsc")
    (shell-command "npm install -g typescript"))

  ;; Docker
  (unless (executable-find "docker-langserver")
    (shell-command "npm install -g dockerfile-language-server-nodejs"))

  ;; YAML
  (unless (executable-find "yaml-language-server")
    (shell-command "npm install -g yaml-language-server"))

  ;; Shell Script
  (unless (executable-find "bash-language-server")
    (shell-command "npm install -g bash-language-server"))

  ;; Astro
  (unless (executable-find "astro-ls")
    (shell-command "npm install -g @astrojs/language-server")))

;; Execute after loading Doom
(add-hook 'doom-after-init-hook #'install-lsp-servers)

;; -------------------------------
;; Magit configuration
;; -------------------------------

(setq magit-blame-heading-format "%-20a %C %s\n") ; Author/date format
(setq magit-refresh-status-buffer t)

(custom-set-faces
 '(magit-blame-hash ((t (:foreground "#7F7F7F"))))) ; Hash color

 (add-hook 'magit-post-push-hook 'magit-refresh-all)
 (add-hook 'magit-post-commit-hook 'magit-refresh-all)
 (add-hook 'magit-post-merge-hook 'magit-refresh-all)
 (add-hook 'magit-post-checkout-hook 'magit-refresh-all)
 (add-hook 'magit-post-fetch-hook 'magit-refresh-all)

(use-package! magit-delta
  :hook (magit-mode . magit-delta-mode)
  :config
  (setq magit-delta-default-options
        '("--side-by-side"
          "--line-numbers"
          "--navigate")))

(after! magit-delta
  (defcustom dan/magit-delta-point-max 50000
    "Maximum length of diff buffer which `magit-delta' will tolerate."
    :group 'magit-delta
    :type  'natnum)
  (defadvice! dan/magit-delta-colorize-maybe (fn &rest args)
    "Disable mode if there are too many characters."
    :around #'magit-delta-call-delta-and-convert-ansi-escape-sequences
    (if (<= (point-max) dan/magit-delta-point-max)
        (apply fn args)
      (magit-delta-mode -1))))

(after! magit
  (add-hook! 'magit-post-refresh-hook
    (when (and (not magit-delta-mode)
               (<= (point-max) dan/magit-delta-point-max))
      (magit-delta-mode +1))))

;; -------------------------------
;; Org-mode configuration
;; -------------------------------

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

(use-package! org-alert
  :after org
  :config
  (setq org-alert-notification-title "🔔 Recordatorio Org"
        org-alert-notify-cutoff 15
        org-alert-notify-after-event-cutoff 5
        org-alert-interval 300
        alert-default-style 'libnotify)

  (org-alert-enable))

(after! org-alert
  (defun my/org-alert--force-high-severity (orig-fn message &rest args)
    (apply orig-fn message
           (plist-put args :severity 'high)))
  (advice-add 'org-alert--notify :around #'my/org-alert--force-high-severity))

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
      '(("w" "Agenda Trabajo"
         ((agenda "" ((org-agenda-files (directory-files-recursively "~/Org/notes/work" "\\.org$"))))
          (todo "" ((org-agenda-files (directory-files-recursively "~/Org/notes/work" "\\.org$"))))))))

;; Adjust indentation
(setq org-edit-src-content-indentation 2)

;; Always present content centered with max 120 characters
(after! org
    (setq-default fill-column 120)
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (add-hook 'org-mode-hook #'visual-fill-column-mode))

(after! org
  ;; Define custom faces
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
    "Face for CANCELLED tasks."))

;; Define keywords and associate faces
(setq org-todo-keywords
      '((sequence
         "TODO(t)"    ;; Pending task
         "NEXT(n)"    ;; Next task to do
         "PROJ(p)"    ;; Project in progress
         "WAIT(w)"    ;; On hold
         "IDEA(i)"    ;; Idea, not yet defined
         "EVENT(e)"   ;; Scheduled event
         "|"
         "DONE(d)"    ;; Finished
         "CANCELLED(c)"))) ;; Cancelled

;; Assign faces (colors and styles) to TODO keywords
(setq org-todo-keyword-faces
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
    ;; Create directory if it doesn't exist
    (make-directory (file-name-directory ruta) t)
    ruta))

;; Capture templates
(after! org
  (setq org-capture-templates
    `(

      ;; Events
      ("a" "Events" entry
       (file+headline "~/Org/agenda/agenda.org" "Events")
       "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:"
       :empty-lines 1 :mkdir t)

       ;; General tasks
      ("t" "General Task" entry
       (file+headline "~/Org/notes/taks.org" "General Task")
       "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1 :mkdir t)

      ;; Personal Notes
      ("p" "Personal Notes" entry
       (file+headline "~/Org/notes/personal/notes.org" "Personal Notes")
       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1 :mkdir t)

      ;; Book Notes (IDEAS)
      ("b" "Book Ideas" entry
       (file+headline "~/Org/notes/book/ideas.org" "Book Ideas")
       "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1 :mkdir t)

      ;; Blog Notes (IDEAS)
      ("h" "Blog Ideas" entry
       (file+headline "~/Org/notes/blog/posts.org" "Blog Ideas")
       "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1 :mkdir t)

      ;; Work Notes
      ("w" "Work Notes" entry
       (file+headline ,(my/org-notas-trabajo-file) "Work Notes")
       "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1)

      ("g" "Games")

      ;; Notes Games
      ("gg" "Game's Notes" entry
       (file+headline "~/Org/notes/games/notes.org" "Game's Notes")
       "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1 :mkdir t)

      ;; Ideas Games
      ("gi" "Game ideas" entry
       (file+headline "~/Org/notes/games/ideas.org" "Game ideas")
       "* IDEA %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
       :empty-lines 1 :mkdir t))))

(defun my/org-mode-set-language ()
  "Sets the Hunspell dictionary according to the #+LANGUAGE option of the Org buffer."
  (when (derived-mode-p 'org-mode)
    (let ((lang (cdr (assoc "LANGUAGE" (org-collect-keywords '("LANGUAGE"))))))
      (when lang
        (setq-local ispell-dictionary (downcase (car lang)))))))

(add-hook 'org-mode-hook #'my/org-mode-set-language)

(after! ox-latex
  (let* ((class-dir (expand-file-name "latex-classes/" doom-user-dir))
         ;; list of themes (name-class . file.cls)
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

;; -------------------------------
;; Blogging configuration
;; -------------------------------

(after! ox-hugo
  ;; Hugo site base path
  (setq org-hugo-base-dir "~/Workspace/blog"
    org-hugo-content-directory "content-org"
    org-hugo-section "posts"
    org-hugo-preserve-filing 'force
    org-hugo-auto-set-lastmod t
    org-hugo-export-with-toc nil
    org-hugo-allow-spaces-in-tags t
    org-hugo-paired-shortcodes "note,warning,tip,details"

    ;; Taxonomies
    org-hugo-taxonomy-tags "tags"
    org-hugo-taxonomy-categories "categories"

    ;; Valid static files
    org-hugo-static-file-extensions
    '("png" "jpg" "jpeg" "gif" "svg" "pdf" "css" "js" "woff" "woff2" "ttf")

    ;; Configured languages for multilingual Hugo
    org-hugo-languages '(("es" . "Spanish")
                         ("en" . "English"))))

;; -------------------------------
;; LaTeX configuration
;; -------------------------------

(after! tex
  ;; Always generate PDF instead of DVI
  (setq TeX-PDF-mode t)

  ;; Enable shell-escape for TikZ, minted, etc.
  (setq TeX-command-extra-options "-shell-escape")

  ;; Clean auxiliary files after compiling
  (add-hook 'TeX-after-TeX-file-run-hook #'TeX-clean)

  ;; Use `latexmk` as default command
  (setq TeX-command-default "LatexMk")

  ;; Enable visual-line mode and flyspell when in LaTeX
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'flyspell-mode)

  ;; Automatically view PDF after compilation
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t))

;; -------------------------------
;; Typescript (NestJS) configuration
;; -------------------------------

(add-to-list 'exec-path "~/.volta/bin")

(use-package! typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq lsp-clients-typescript-auto-install-server t  ; Install automatically
        lsp-clients-typescript-log-verbosity "debug"  ; Detailed logs
        lsp-completion-enable-auto-import t           ; Auto-imports
        lsp-tsserver-plugins-path "~/node_modules"))    ; TS plugins path

(setq lsp-clients-typescript-init-opts '(:plugins ["@nestjs/swagger-plugin"]
                                         :compilerOptions {:experimentalDecorators t}))

(setq prettier-js-args '("--trailing-comma" "all"
                         "--single-quote" "true")
      eslintd-fix-mode t)  ; Fix errors on save

;; -------------------------------
;; Containers configuration
;; -------------------------------

;; Associate docker-compose.yml or docker-compose.yaml files with yaml-mode
(add-to-list 'auto-mode-alist
             '("docker-compose[.-]?\\(yml\\|yaml\\)\\'" . yaml-mode))

;; -------------------------------
;; Dired configuration
;; -------------------------------

;; Enable hidden and asynchronous modes in dired
(add-hook! dired-mode
  (dired-hide-details-mode)
  (dired-async-mode))

;; Configure dired-subtree
(use-package! dired-subtree
  :after dired)

;; Configure dired when loaded
(after! dired
  ;; Custom shortcuts for asynchronous operations
  (map! :map dired-mode-map
        :n "N" #'dired-create-empty-file
        :n "R" #'dired-async-do-rename
        :n "C" #'dired-async-do-copy
        :n "S" #'dired-async-do-symlink
        :n "H" #'dired-async-do-hardlink)

  ;; Enable smart target based on recent files
  (setq dired-dwim-target #'dired-dwim-target-recent))

;; Configure dirvish
(after! dirvish
  (setq dirvish-default-layout '(0 0 0.4)
        dirvish-layout-recipes '((1 0.11 0.55)
                                 (0 0    0.40)))
  ;; Ensure 'file-size' is in attributes
  (pushnew! dirvish-attributes 'file-size))

;; Shortcut to open Dired from leader
(map! :leader
      :desc "Dired" "o -" #'dired-jump)

;; -------------------------------
;; Markdown configuration
;; -------------------------------

(use-package! markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;; -------------------------------
;; Web configuration
;; -------------------------------
(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

(set-file-template! "\\.astro$" :trigger "__astro" :mode 'web-mode)
(set-file-template! "\\.svelte$" :trigger "__svelte" :mode 'web-mode)

;; -------------------------------
;; KDL configuration
;; -------------------------------

(use-package! kdl-mode
  :mode "\\.kdl\\'")

;; -------------------------------
;; Company configuration
;; -------------------------------

(after! company
  (setq company-minimum-prefix-length 2
   company-idle-delay 0.05))

(use-package! consult-company
  :commands consult-company)

(after! (company consult)
    (map! :map company-active-map
     "C-S-s" #'consult-company))

(after! consult-dir
  (defun consult-dir--fasd-dirs ()
    "Return list of fasd dirs."
    (split-string (shell-command-to-string "fasd -ld") "\n" t))

  (defvar consult-dir--source-fasd
    `(:name     "Fasd dirs"
      :narrow   ?f
      :category file
      :face     consult-file
      :history  file-name-history
      :enabled  ,(lambda () (executable-find "fasd"))
      :items    ,#'consult-dir--fasd-dirs)
    "Fasd directory source for `consult-dir'.")

  (add-to-list 'consult-dir-sources 'consult-dir--source-fasd t))

(use-package! consult-projectile
  :defer t
  :init
  (map! :leader
        :desc "Project find" "SPC" #'consult-projectile))

(after! consult-projectile
    (setq consult-projectile-use-projectile-switch-project t))

;; -------------------------------
;; Buffers configuration
;; -------------------------------

(use-package! cus-edit
  :defer t
  :custom
  (custom-unlispify-menu-entries nil)
  (custom-unlispify-tag-names nil)
  (custom-unlispify-remove-prefixes nil))

(setq uniquify-buffer-name-style 'forward)

;; -------------------------------
;; Detached configuration
;; -------------------------------

(use-package! detached
  :after-call (compile dired dired-rsync embark-act eshell org-mode projectile-mode shell vterm)
  :init
  ;; Initialize Detached on load
  (add-hook! 'doom-first-buffer-hook #'detached-init)
  :custom
  ;; OS-adapted notifications
  (detached-notification-function
   (if IS-LINUX
       #'detached-state-transition-notifications-message
     #'detached-extra-alert-notification))
  ;; Base directories
  (detached-db-directory doom-cache-dir)
  (detached-session-directory (temporary-file-directory))
  ;; Block auto-init for some modes
  (detached-init-block-list '(dired-rsync dired))
  :config
  ;; Direct shortcut to execute detached commands
  (map! :g "M-&" #'detached-shell-command))

;; Consult UI for Detached
(use-package! detached-consult
  :defer t
  :init
  (map! :leader
        :desc "Detached Sessions" :g "o s" #'detached-consult-session))

;; Detached compilations
(use-package! detached-compile
  :defer t
  :init
  (map! :leader
        :desc "Compile (Detached)"   :g "c c" #'detached-compile
        :desc "Recompile (Detached)" :g "c C" #'detached-compile-recompile))

;; Active sessions list
(use-package! detached-list
  :defer t
  :init
  (map! :leader
        :desc "Detached Manage Sessions" :g "o S" #'detached-list-sessions)
  :config
  ;; Emacs state for sessions list (avoid Evil mode)
  (evil-set-initial-state 'detached-list-mode 'emacs))

;; -----------------------------------
;; Large files configuration
;; -----------------------------------

(use-package! vlf
  :defer-incrementally t
  :custom
  (vlf-batch-size-remote read-process-output-max)
  :config
  (require 'vlf-setup)
  (add-hook! 'vlf-mode-hook #'so-long-mode))

(after! so-long
  (add-to-list 'so-long-mode-preserved-variables 'vlf-mode))

;; Enable global word wrap
(add-hook! 'doom-first-buffer-hook #'+global-word-wrap-mode)

;; -------------------------------
;; RSS configuration
;; -------------------------------

(add-hook! 'elfeed-search-mode-hook #'elfeed-update)
(make-directory "~/.elfeed" t)

(after! elfeed
    (setq elfeed-search-filter "@7-days-ago +unread")
  (setq elfeed-org-allow-http-feeds t))

;; Set org feed file
(setq rmh-elfeed-org-files '("~/.config/doom/elfeed/elfeeds.org"))

;; Load elfeed-download package
(after! elfeed
  (load! "lisp/elfeed-download")
  (require 'elfeed-org)
  (elfeed-org)
  (elfeed-download-setup))

;; Configure elfeed - consolidate all elfeed config in one after! block
(after! elfeed
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-search-filter "@1-week-ago +unread")

  ;; Set up elfeed-download
  (elfeed-download-setup)

  ;; Key bindings
  (map! :map elfeed-search-mode-map
        :n "d" #'elfeed-download-current-entry
        :n "O" #'elfeed-search-browse-url))

;; Update hourly
(run-at-time nil (* 60 60) #'elfeed-update)

;; Load elfeed-download package
(after! elfeed
  (load! "lisp/elfeed-download")
  (require 'elfeed-org)
  (elfeed-org)
  (elfeed-download-setup))

;; Elfeed-tube configuration
(use-package! elfeed-tube
  :after elfeed
  :config
  (elfeed-tube-setup)
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

(setq elfeed-tube-mpv-mpv-options '("--force-window=yes"))

;; Reading the feeds
(setq-default fill-column 120)
(setq visual-fill-column-width 120
      visual-fill-column-center-text t)
(add-hook 'elfeed-show-mode-hook #'visual-fill-column-mode)

;; Custom keymaps
(map! :leader
      ;; Mappings for Elfeed 
      (:prefix("e" . "Elfeed")
       :desc "Open elfeed"              "e" #'elfeed
       :desc "Open ERC"                 "r" #'my/erc-connect
       :desc "Open EWW Browser"         "w" #'eww
       :desc "Update elfeed"            "u" #'elfeed-update
       :desc "MPV watch video"          "v" #'elfeed-tube-mpv
       ))

;; -------------------------------
;; PDFs configuration
;; -------------------------------

(use-package! pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
                                        (blink-cursor-mode -1)
                                        (doom-modeline-mode -1)))

;; -------------------------------
;; Emms configuration
;; -------------------------------

(emms-all)
(emms-default-players)
(emms-mode-line-mode 1)
(emms-playing-time-mode 1)

(setq emms-track-description-function
      (lambda (track)
        (let ((artist (emms-track-get track 'info-artist))
              (title (emms-track-get track 'info-title)))
          (if (and artist title)
              (format "%s - %s" artist title)
            (file-name-nondirectory (emms-track-name track))))))

(setq emms-source-file-default-directory "~/Music"
      emms-browser-covers #'emms-browser-cache-thumbnail-async
      emms-browser-thumbnail-small-size 64
      emms-browser-thumbnail-medium-size 128
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

;; Nord theme colors
(with-eval-after-load 'emms
  (custom-set-faces
   ;; Nord colors: https://www.nordtheme.com/docs/colors-and-palettes
   '(emms-browser-artist-face ((t (:foreground "#ECEFF4" :height 1.1))))  ;; Nord Snow Storm (bright white)
   '(emms-browser-album-face ((t (:foreground "#88C0D0" :height 1.0))))   ;; Nord Frost (blue)
   '(emms-browser-track-face ((t (:foreground "#A3BE8C" :height 1.0))))   ;; Nord Aurora (green)
   '(emms-playlist-track-face ((t (:foreground "#D8DEE9" :height 1.0))))  ;; Nord Snow Storm (lighter white)
   '(emms-playlist-selected-face ((t (:foreground "#BF616A" :weight bold))))))  ;; Nord Aurora (red)

(map! :leader
      (:prefix ("m" . "music/EMMS")  ;; Changed from 'a' to 'm' for music
       :desc "Play at directory tree"   "d" #'emms-play-directory-tree
       :desc "Go to emms playlist"      "p" #'emms-playlist-mode-go
       :desc "Shuffle"                  "h" #'emms-shuffle
       :desc "Emms pause track"         "x" #'emms-pause
       :desc "Emms stop track"          "s" #'emms-stop
       :desc "Emms play previous track" "b" #'emms-previous
       :desc "Emms play next track"     "n" #'emms-next))

;; Grab album artwork for dunst to display
(defun emms-cover-art-path ()
  "Return the path of the cover art for the current track."
  (let* ((track (emms-playlist-current-selected-track))
         (path (emms-track-get track 'name))
         (dir (file-name-directory path))
         (cover-files (directory-files dir nil ".*\\(jpg\\|png\\|jpeg\\)$")))
    (when cover-files
      (concat dir (car cover-files)))))

(defvar my/emms-default-cover "~/.config/doom/images/kmix.svg")

(defun my/emms-extract-cover (track)
  "Extract embedded cover art from TRACK to a temporary file, if it exists. Returns path or nil."
  (let ((file (emms-track-get track 'name))
        (output (make-temp-file "emms-cover-" nil ".jpg")))
    (when (and file (string-match-p "\\.mp3\\|\\.flac\\|\\.m4a" file))
      (let ((exit-code (call-process "ffmpeg" nil nil nil
                                     "-y"                     ; Overwrite
                                     "-i" file
                                     "-an"                    ; No audio
                                     "-vcodec" "copy"
                                     output)))
        (if (and (file-exists-p output)
                 (= exit-code 0))
            output
          nil)))))

(defun my/emms-show-alert ()
  "Show alert with music metadata and cover if available."
  (when-let* ((track (emms-playlist-current-selected-track))
              (artist (emms-track-get track 'info-artist))
              (title (emms-track-get track 'info-title))
              (album (emms-track-get track 'info-album)))
    (let ((cover (or (my/emms-extract-cover track)
                     my/emms-default-cover)))
      (alert (format "%s\n%s" artist album)
             :title (format "🎵 %s" title)
             :icon cover
             :category "music"))))

(add-hook 'emms-player-started-hook #'my/emms-show-alert)

;; -------------------------------
;; Deft configuration
;; -------------------------------

;; Configure notes directory
(setq deft-directory "~/Org/")  ; Path where notes are saved

;; Search in subdirectories
(setq deft-recursive t)

;; Use .org extension by default
(setq deft-extensions '("org" "md" "txt"))  ; Allowed formats
(setq deft-default-extension "org")         ; Default extension

;; Ignore certain files (e.g., Org temporary files)
(setq deft-ignore-file-regexp "\\.#\\|~$")

;; -------------------------------
;; Readers configuration
;; -------------------------------

(setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Library"))
        calibredb-format-all-the-icons t)

  ;; Set up key bindings for calibredb-search-mode
  (map! :map calibredb-search-mode-map
        :n "RET" #'calibredb-find-file
        :n "?" #'calibredb-dispatch
        :n "a" #'calibredb-add
        :n "d" #'calibredb-remove
        :n "j" #'calibredb-next-entry
        :n "k" #'calibredb-previous-entry
        :n "l" #'calibredb-open-file-with-default-tool
        :n "s" #'calibredb-set-metadata-dispatch
        :n "S" #'calibredb-switch-library
        :n "q" #'calibredb-search-quit))

;; -------------------------------
;; Notifications configuration
;; -------------------------------

(after! alert
  (defun my/alert-sound-wrapper (orig-fn message &rest args)
    (apply orig-fn message args)
    (let* ((severity (plist-get args :severity))
           (sound (pcase severity
                    ('high "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
                    ('moderate "/usr/share/sounds/freedesktop/stereo/message.oga")
                    ('normal "/usr/share/sounds/freedesktop/stereo/dialog-information.oga")
                    ('low "/usr/share/sounds/freedesktop/stereo/complete.oga"))))
      (ignore-errors
        (start-process "alert-sound" nil "paplay" sound))))

  (advice-add 'alert :around #'my/alert-sound-wrapper))

(use-package! alert
  :defer t
  :custom
  (alert-default-style (if IS-LINUX 'libnotify 'osx-notifier)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (alert "Welcome, Master" :title "Doom Emacs Awaits" :severity 'normal)))

(defun my/org-export-pdf-alert (&rest _args)
  (alert "Org to PDF export completed" :title "Org Export" :severity 'normal))

(advice-add 'org-latex-export-to-pdf :after #'my/org-export-pdf-alert)

(defun my/latex-compilation-alert (filename)
  (alert "LaTeX to PDF compilation completed" :title "Latex Compile" :severity 'normal))

(add-hook 'TeX-after-compilation-finished-functions #'my/latex-compilation-alert)
