;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Encoding configuration
;; -------------------------------

(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)

(setq highlight-escape-sequences nil)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (boundp 'show-trailing-whitespace)
              (setq show-trailing-whitespace nil))
            (when (boundp 'whitespace-mode)
              (whitespace-mode -1))))

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
(setq gc-cons-threshold (* 16 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq comp-deferred-compilation t)
(setq comp-async-jobs-number 8)

;; Garbage collector optimization
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 128 1024 1024))

;; Restore gc threshold after startup for better memory recovery
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024))))

;; Fix x11 issues
(setq x-no-window-manager t)
(setq frame-inhibit-implied-resize t)
(setq focus-follows-mouse nil)

;; Start emacs maximized
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defvar +my/doom-dir (expand-file-name "~/.config/doom")
  "Doom configuration directory.")

;; Setup custom splashscreen
(setq fancy-splash-image (expand-file-name "images/gnu_color.png" +my/doom-dir))

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Welcome Home, Master.")))

;; Blink cursor
(blink-cursor-mode 1)

;; -------------------------------
;; Theme configuration
;; -------------------------------

;; Maintain terminal transparency in Doom Emacs
(after! doom-themes
  (unless (display-graphic-p)
    (set-face-background 'default "undefined")))

;; remove top frame bar in emacs
(add-to-list 'default-frame-alist '(undecorated . t))

;; Transparency
;; (set-frame-parameter (selected-frame) 'alpha '(94 . 97))
;; (add-to-list 'default-frame-alist '(alpha . (94 . 97)))

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
    '(mode-line :height 130 :inherit 'variable-pitch)
    '(mode-line-inactive :height 120 :inherit 'variable-pitch))

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
;; Dictionary configuration
;; -------------------------------

(setq flyspell-lazy-idle-seconds 0.3)

(defun my/change-ispell-dictionary (lang)
  "Change Flyspell dictionary to LANG."
  (interactive "sLanguage (e.g., es_ES or en_US): ")
  (setq ispell-dictionary lang)
  (ispell-change-dictionary lang)
  (message "Dictionary changed to %s" lang))

(map! :leader
      (:prefix ("D" . "dictionary")
       :desc "Change dictionary"         "c" #'my/change-ispell-dictionary
       :desc "Check word at point"     "w" #'ispell-word
       :desc "Toggle flyspell"          "s" #'flyspell-mode
       :desc "Check buffer"             "b" #'flyspell-buffer
       :desc "Next error"              "n" #'flyspell-goto-next-error
       :desc "Add word to dictionary"   "a" #'+spell/add-word))

;; -------------------------------
;; Directories configuration (lazy creation)
;; -------------------------------

(defvar +my/org-base-dir (expand-file-name "~/Org")
  "Base directory for Org files.")

(defvar +my/org-notes-dir (expand-file-name "notes" +my/org-base-dir)
  "Directory for Org notes.")

(defvar +my/org-agenda-dir (expand-file-name "agenda" +my/org-base-dir)
  "Directory for Org agenda files.")

(defvar +my/org-work-dir (expand-file-name "work" +my/org-notes-dir)
  "Directory for work-related notes.")

(defvar +my/org-journal-dir (expand-file-name "journal" +my/org-base-dir)
  "Directory for Org journal.")

(defvar +my/workspace-dir (expand-file-name "~/Workspace")
  "Workspace base directory.")

(defvar +my/library-dir (expand-file-name "~/Library")
  "Library base directory.")

(setq my/directories
      (append
       (list +my/org-base-dir
             (expand-file-name "journal" +my/org-base-dir)
             (expand-file-name "journal/personal" +my/org-base-dir)
             (expand-file-name "journal/work" +my/org-base-dir)
             +my/org-agenda-dir
             +my/org-notes-dir
             (expand-file-name "notes/blog" +my/org-notes-dir)
             (expand-file-name "notes/book" +my/org-notes-dir)
             (expand-file-name "notes/personal" +my/org-notes-dir)
             +my/org-work-dir
             (expand-file-name "notes/games" +my/org-notes-dir))
       (list +my/library-dir
             +my/workspace-dir
             (expand-file-name "Books" +my/workspace-dir)
             (expand-file-name "Books/latex" +my/workspace-dir)
             (expand-file-name "Books/org" +my/workspace-dir)
             (expand-file-name "Blog" +my/workspace-dir)
             (expand-file-name "Personal" +my/workspace-dir)
             (expand-file-name "Scripts" +my/workspace-dir)
             (expand-file-name "Work" +my/workspace-dir)
             (expand-file-name "Games" +my/workspace-dir))))

(defun +my/create-directories (&optional force)
  "Create configured directories. With FORCE, create even if exist."
  (interactive "P")
  (dolist (directory my/directories)
    (when (or force (not (file-directory-p directory)))
      (make-directory directory t)
      (message "Created directory: %s" directory))))

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

(add-to-list 'exec-path (expand-file-name "~/.volta/bin"))

(use-package! typescript-mode
  :init
  (setq lsp-clients-typescript-auto-install-server t
        lsp-clients-typescript-log-verbosity "debug"
        lsp-completion-enable-auto-import t
        lsp-clients-typescript-init-opts '(:plugins ["@nestjs/swagger-plugin"]
                                          :compilerOptions {:experimentalDecorators t})
        lsp-tsserver-plugins-path (expand-file-name "~/node_modules"))
  :hook (typescript-mode . lsp-deferred))

(use-package! prettier-js
  :init
  (setq prettier-js-args '("--trailing-comma" "all"
                          "--single-quote" "true"))
  :config
  (setq eslintd-fix-mode t))

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
                                 (0 0    0.40))
        dirvish-attributes '(vc-state subtree-state nerd-icons file-size)))

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

;; Set org feed file
(setq rmh-elfeed-org-files (list (expand-file-name "elfeed/elfeeds.org" +my/doom-dir)))

(after! elfeed
  (setq elfeed-search-filter "@7-days-ago +unread")
  (setq elfeed-org-allow-http-feeds t)
  (setq elfeed-db-directory (expand-file-name "~/.elfeed"))
  (load! "lisp/elfeed-download")
  (require 'elfeed-org)
  (elfeed-org)
  (elfeed-download-setup)
  (map! :map elfeed-search-mode-map
        :n "d" #'elfeed-download-current-entry
        :n "O" #'elfeed-search-browse-url))

;; Update hourly
(run-at-time nil (* 60 60) #'elfeed-update)

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
       :desc "Open EWW Browser"         "w" #'eww
       :desc "Update elfeed"            "u" #'elfeed-update
       :desc "MPV watch video"          "v" #'elfeed-tube-mpv
       ))

;;; Load external configuration files
(load! "lisp/lsp")
(load! "lisp/org")
(load! "lisp/media")
(load! "lisp/ia")
