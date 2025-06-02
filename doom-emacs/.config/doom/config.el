;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Configuración visual y comportamiento
;; -------------------------------

;; Iniciar Emacs maximizado
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Setup custom splashscreen
;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(setq fancy-splash-image "~/.config/doom/Logos/gnu_color.png")

(add-hook! '+doom-dashboard-functions :append
  (insert "\n" (+doom-dashboard--center +doom-dashboard--width "Welcome Home, Master.")))

;; Blink cursor
(blink-cursor-mode 1)

;; Tema
(setq doom-theme 'doom-nord)
;; (setq doom-theme 'doom-gruvbox)

;; Maintain terminal transparency in Doom Emacs
(after! doom-themes
  (unless (display-graphic-p)
    (set-face-background 'default "undefined")))

;; remove top frame bar in emacs
(add-to-list 'default-frame-alist '(undecorated . t))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(96 . 97))
(add-to-list 'default-frame-alist '(alpha . (96 . 97)))

;; Evil-escape sequence
(setq-default evil-escape-key-sequence "kj")
(setq-default evil-escape-delay 0.1)

; Don't move cursor back when exiting insert mode
(setq evil-move-cursor-back nil)
;; granular undo with evil mode
(setq evil-want-fine-undo t)

(setq which-key-idle-delay 0.2)

;; Números de línea relativos
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Modelo de línea y fuentes
(custom-set-faces!
    '(mode-line :height 90 :inherit 'variable-pitch)
    '(mode-line-inactive :height 80 :inherit 'variable-pitch))

(after! all-the-icons
    (setq all-the-icons-scale-factor 1.5))

;; Tabs estilo IDE (Centaur Tabs)
(setq centaur-tabs-style "alternate"
      centaur-tabs-height 32
      centaur-tabs-set-bar 'under)

(centaur-tabs-mode t)

;; Folding para lenguajes sin soporte nativo
(add-hook 'sh-mode-hook #'outline-minor-mode) ; Para Shell Script
(add-hook 'markdown-mode-hook #'outline-minor-mode)

;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(setq hs-isearch-open t)           ; Expandir folds al buscar

(after! highlight-indent-guides
    (setq highlight-indent-guides-method (if (display-graphic-p)
                                             'bitmap
                                           'character)
     highlight-indent-guides-responsive 'top))

(add-transient-hook! 'doom-first-input-hook
  (let ((cell (assoc 'side
                     (assoc "^\\*\\(?:Wo\\)?Man " display-buffer-alist))))
    (setcdr cell 'right)))

(setq require-final-newline 'ask)

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

;; Mejoramos el manual
(add-transient-hook! 'doom-first-input-hook
  (let ((cell (assoc 'side
                     (assoc "^\\*\\(?:Wo\\)?Man " display-buffer-alist))))
    (setcdr cell 'right)))

(use-package! dotenv-mode
  :mode ("\\.env\\.?.*\\'" . dotenv-mode))

(setq comment-tabs t)  ; Alinea comentarios con tabs o espacios

;; Indentación y formato de código
(setq-default tab-width 2)

;; -------------------------------
;; Configuración modeline
;; -------------------------------

(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-lsp-icon t)
(setq doom-modeline-major-mode-color-icon t)

;; Mostrar íconos en la modeline
(setq doom-modeline-icon t)

;; Mostrar el nombre del proyecto y del archivo
;; (setq doom-modeline-project-detection 'auto)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; Mostrar posición en el buffer
(setq doom-modeline-percent-position t)

;; Mostrar el entorno de lsp y checker si están activos
(setq doom-modeline-lsp t)

;; Mostrar hora
(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(display-time-mode 1)

;; Mostrar batería
(display-battery-mode 1)

;; Mostrar el workspace
(setq doom-modeline-workspace-name t)

;; -------------------------------
;; Configuración de diccionarios
;; -------------------------------

;; Función para cambiar el diccionario de Flyspell
(defun my/change-ispell-dictionary (lang)
  "Cambia el diccionario de Flyspell a LANG."
  (interactive "sIdioma (por ejemplo, es_ES o en_US): ")
  (setq ispell-dictionary lang)
  (ispell-change-dictionary lang)
  (message "Diccionario cambiado a %s" lang))

;; Añadir un atajo de teclado para cambiar de diccionario fácilmente
(map! :leader
      :desc "Cambiar diccionario Flyspell" "l d" #'my/change-ispell-dictionary)

;; -------------------------------
;; Configuración de directorios
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
                       "~/Workspace/books"
                       "~/Workspace/books/latex"
                       "~/Workspace/books/org"
                       "~/Workspace/blog"
                       "~/Workspace/work"
                       "~/Workspace/games"))

(dolist (directory my/directories)
  (unless (file-directory-p directory)
    (make-directory directory t)))

;; -------------------------------
;; Configuración de lisp
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
;; Configuración de escritura
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
;; Configuración de Modo Zen
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
;; Configuración de LSP para todos los lenguajes
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
(setq lsp-ui-sideline-enable t                ; Información en barra lateral
      lsp-headerline-breadcrumb-enable t      ; Ruta del archivo en headerline
      company-minimum-prefix-length 1
      company-idle-delay 0.1)

;; Activar auto-completado y acciones de código
(setq lsp-enable-suggest-server-download t  ; Descargar LSP si no está instalado
      lsp-auto-execute-action t             ; Ejecutar acciones automáticamente (opcional)
      lsp-completion-enable t)               ; Integrar con company-mode
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

;; Usar el entorno virtual para Python
(setq lsp-pyright-venv-path "~/.emacs-lsp-venv")
(setq python-shell-interpreter "~/.emacs-lsp-venv/bin/python3")

;; Python (Black)
(setq format-all-formatters '(("Python" black)))

;; JavaScript/TypeScript (Prettier)
(setq prettier-js-args '("--trailing-comma" "all"
                         "--single-quote" "true"))

;; Shell (shfmt)
(setq shfmt-arguments "-i=2"  ; Sangría de 2 espacios
      shfmt-path "~/.go/bin/shfmt")

;; Lisp (indentación estándar de Emacs)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)  ; Soporte de documentación

;; Activar formateo automático al guardar
(setq +format-on-save-enabled-modes
      '(python-mode js-mode typescript-mode c-mode c++-mode sh-mode lua-mode))

(defun install-lsp-servers ()
  "Instala todos los servidores LSP requeridos"
  (interactive)

  ;; Python LSP
  (unless (file-exists-p "~/.emacs-lsp-venv/bin/pylsp")
    (shell-command "python3 -m venv ~/.emacs-lsp-venv")
    (shell-command "~/.emacs-lsp-venv/bin/pip install python-lsp-server pyright pylint black"))

  ;; JavaScript/TypeScript
  (unless (executable-find "typescript-language-server")
    (shell-command "npm install -g typescript-language-server vscode-langservers-extracted"))

  ;; Docker
  (unless (executable-find "docker-langserver")
    (shell-command "npm install -g dockerfile-language-server-nodejs"))

  ;; YAML
  (unless (executable-find "yaml-language-server")
    (shell-command "npm install -g yaml-language-server"))

  ;; Shell Script
  (unless (executable-find "bash-language-server")
    (shell-command "npm install -g bash-language-server")))

;; Ejecutar después de cargar Doom
(add-hook 'doom-after-init-hook #'install-lsp-servers)

;; -------------------------------
;; Configuración de magit
;; -------------------------------

(setq magit-blame-heading-format "%-20a %C %s\n") ; Formato de autor/fecha
(setq magit-refresh-status-buffer t)

(custom-set-faces
 '(magit-blame-hash ((t (:foreground "#7F7F7F"))))) ; Color del hash

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
;; Configuración de org-mode
;; -------------------------------

(setq org-directory "~/Org"
      org-roam-directory "~/Org/notes"
      org-journal-dir "~/Org/journal"
      org-startup-indented t)

(use-package org
  :ensure nil
  :custom (org-modules '(org-habit)))

(after! org
  (map! :map org-mode-map
        :n "<M-left>" #'org-do-promote
        :n "<M-right>" #'org-do-demote))

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
  "Abrir o crear la entrada de diario laboral para hoy."
  (let ((org-journal-dir "~/Org/journal/work/")
        (org-journal-file-format "%Y-%m-%d.org")
        (org-journal-date-format "%A, %d %B %Y")
        (org-journal-date-prefix "#+TITLE: ")
        (org-journal-enable-agenda-integration nil))
    (org-journal-new-entry t)))

(map! :leader
      (:prefix ("n j" . "journal")
       :desc "Abrir diario personal" "p" #'org-journal-new-entry
       :desc "Abrir diario laboral"  "w" #'my/org-journal-open-work))

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

;; Ajustar sangría
(setq org-edit-src-content-indentation 2)

;; Presentamos siempre el contenido en el centro con un máximo de 120 caracteres
(after! org
    (setq-default fill-column 120)
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (add-hook 'org-mode-hook #'visual-fill-column-mode))

(after! org
  ;; Definir caras personalizadas
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

;; Definir keywords y asociarles las caras
(setq org-todo-keywords
      '((sequence
         "TODO(t)"    ;; Tarea pendiente
         "NEXT(n)"    ;; Siguiente tarea a hacer
         "PROJ(p)"    ;; Proyecto en progreso
         "WAIT(w)"    ;; En espera
         "IDEA(i)"    ;; Idea, aún no definida
         "EVENT(e)"   ;; Evento programado
         "|"
         "DONE(d)"    ;; Finalizado
         "CANCELLED(c)"))) ;; Cancelado

;; Asignar caras (colores y estilos) a las keywords TODO
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
  "Devuelve una ruta de archivo basada en la fecha actual para notas de trabajo."
  (let* ((fecha (format-time-string "%Y/%m/%d-note.org"))
         (ruta (expand-file-name fecha "~/Org/notes/work/")))
    ;; Crear directorio si no existe
    (make-directory (file-name-directory ruta) t)
    ruta))

;; Plantillas de captura
(after! org
  (setq org-capture-templates
    `(

      ;; Events
      ("a" "Events" entry
       (file+headline "~/Org/agenda/agenda.org" "Events")
       "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:"
       :empty-lines 1 :mkdir t)

      ;; Tareas generales
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
       (file+headline "~/Org/notes/blog/posts.org" "Book Ideas")
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
  "Establece el diccionario de Hunspell según la opción #+LANGUAGE del buffer Org."
  (when (derived-mode-p 'org-mode)
    (let ((lang (cdr (assoc "LANGUAGE" (org-collect-keywords '("LANGUAGE"))))))
      (when lang
        (setq-local ispell-dictionary (downcase (car lang)))))))

(add-hook 'org-mode-hook #'my/org-mode-set-language)

;; -------------------------------
;; Configuración de blogging
;; -------------------------------

(after! ox-hugo
  ;; Ruta base del sitio Hugo
  (setq org-hugo-base-dir "~/Workspace/blog-hugo"
    org-hugo-content-directory "content-org"
    org-hugo-section "posts"
    org-hugo-preserve-filing 'force
    org-hugo-auto-set-lastmod t
    org-hugo-export-with-toc nil
    org-hugo-allow-spaces-in-tags t
    org-hugo-paired-shortcodes "note,warning,tip,details"

    ;; Taxonomías
    org-hugo-taxonomy-tags "tags"
    org-hugo-taxonomy-categories "categories"

    ;; Archivos estáticos válidos
    org-hugo-static-file-extensions
    '("png" "jpg" "jpeg" "gif" "svg" "pdf" "css" "js" "woff" "woff2" "ttf")

    ;; Idiomas configurados para Hugo multilingüe
    org-hugo-languages '(("es" . "Spanish")
                         ("en" . "English"))))

;; -------------------------------
;; Configuración de LaTeX
;; -------------------------------

(after! tex
  ;; Generar siempre PDF en lugar de DVI
  (setq TeX-PDF-mode t)

  ;; Habilitar shell-escape para TikZ, minted, etc.
  (setq TeX-command-extra-options "-shell-escape")

  ;; Limpiar archivos auxiliares después de compilar
  (add-hook 'TeX-after-TeX-file-run-hook #'TeX-clean)

  ;; Usar `latexmk` como comando por defecto
  (setq TeX-command-default "LatexMk")

  ;; Activar modo visual-line y flyspell cuando estés en LaTeX
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  (add-hook 'TeX-mode-hook #'flyspell-mode)

  ;; Ver automáticamente el PDF tras la compilación
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t))

;; -------------------------------
;; Configuración de Typescript (NestJS)
;; -------------------------------

(add-to-list 'exec-path "~/.volta/bin")

(use-package! typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq lsp-clients-typescript-auto-install-server t  ; Instalar automáticamente
        lsp-clients-typescript-log-verbosity "debug"  ; Logs detallados
        lsp-completion-enable-auto-import t           ; Auto-imports
        lsp-tsserver-plugins-path "~/node_modules"))    ; Ruta de plugins de TS

(setq lsp-clients-typescript-init-opts '(:plugins ["@nestjs/swagger-plugin"]
                                         :compilerOptions {:experimentalDecorators t}))

(setq prettier-js-args '("--trailing-comma" "all"
                         "--single-quote" "true")
      eslintd-fix-mode t)  ; Corregir errores al guardar

;; -------------------------------
;; Configuración Contenedores
;; -------------------------------

;; Asociar archivos docker-compose.yml o docker-compose.yaml con yaml-mode
(add-to-list 'auto-mode-alist
             '("docker-compose[.-]?\\(yml\\|yaml\\)\\'" . yaml-mode))

;; -------------------------------
;; Configuración de dired
;; -------------------------------

;; Activar modos ocultos y asincrónicos en dired
(add-hook! dired-mode
  (dired-hide-details-mode)
  (dired-async-mode))

;; Configurar dired-subtree
(use-package! dired-subtree
  :after dired)

;; Configurar dired cuando esté cargado
(after! dired
  ;; Atajos personalizados para operaciones asincrónicas
  (map! :map dired-mode-map
        :n "N" #'dired-create-empty-file
        :n "R" #'dired-async-do-rename
        :n "C" #'dired-async-do-copy
        :n "S" #'dired-async-do-symlink
        :n "H" #'dired-async-do-hardlink)

  ;; Activar destino inteligente basado en archivos recientes
  (setq dired-dwim-target #'dired-dwim-target-recent))

;; Configurar dirvish
(after! dirvish
  (setq dirvish-default-layout '(0 0 0.4)
        dirvish-layout-recipes '((1 0.11 0.55)
                                 (0 0    0.40)))
  ;; Asegurar que 'file-size esté en los atributos
  (pushnew! dirvish-attributes 'file-size))

;; Atajo para abrir Dired desde el líder
(map! :leader
      :desc "Dired" "o -" #'dired-jump)

;; -------------------------------
;; Configuración de markdown
;; -------------------------------

(use-package! markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))

;; -------------------------------
;; Configuración de company
;; -------------------------------

;; (use-package! cape
;;   :init
;;   (after! term
;;       (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
;;     (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
;;   :defer t)

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
;; Configuración de buffers
;; -------------------------------

(use-package! cus-edit
  :defer t
  :custom
  (custom-unlispify-menu-entries nil)
  (custom-unlispify-tag-names nil)
  (custom-unlispify-remove-prefixes nil))

(setq uniquify-buffer-name-style 'forward)

;; -------------------------------
;; Configuración de detached
;; -------------------------------

(use-package! detached
  :after-call (compile dired dired-rsync embark-act eshell org-mode projectile-mode shell vterm)
  :init
  ;; Inicializar Detached al cargar
  (add-hook! 'doom-first-buffer-hook #'detached-init)
  :custom
  ;; Notificaciones adaptadas a OS
  (detached-notification-function
   (if IS-LINUX
       #'detached-state-transition-notifications-message
     #'detached-extra-alert-notification))
  ;; Directorios base
  (detached-db-directory doom-cache-dir)
  (detached-session-directory (temporary-file-directory))
  ;; Bloquear auto-init para algunos modos
  (detached-init-block-list '(dired-rsync dired))
  :config
  ;; Atajo directo para ejecutar comandos desacoplados
  (map! :g "M-&" #'detached-shell-command))

;; Consult UI para Detached
(use-package! detached-consult
  :defer t
  :init
  (map! :leader
        :desc "Detached Sessions" :g "o s" #'detached-consult-session))

;; Compilaciones desacopladas
(use-package! detached-compile
  :defer t
  :init
  (map! :leader
        :desc "Compile (Detached)"   :g "c c" #'detached-compile
        :desc "Recompile (Detached)" :g "c C" #'detached-compile-recompile))

;; Lista de sesiones activas
(use-package! detached-list
  :defer t
  :init
  (map! :leader
        :desc "Detached Manage Sessions" :g "o S" #'detached-list-sessions)
  :config
  ;; Emacs state para lista de sesiones (evitar modo Evil)
  (evil-set-initial-state 'detached-list-mode 'emacs))

;; -----------------------------------
;; Configuración archivos grandes
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

;; Activar word wrap global
(add-hook! 'doom-first-buffer-hook #'+global-word-wrap-mode)

;; -------------------------------
;; Configuración de RSS
;; -------------------------------

(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

(after! elfeed
    (setq elfeed-search-filter "@7-days-ago +unread")
  (setq elfeed-org-allow-http-feeds t))

(after! elfeed-org
    (setq rmh-elfeed-org-files (list "~/Org/rss/elfeeds.org"))
  ;; Crear estructura inicial si no existe
  (unless (file-exists-p (concat my/rss-base-dir "elfeeds.org"))
    (with-temp-file (concat my/rss-base-dir "elfeeds.org")
      (insert "#+TITLE: Gestión de Feeds\n\n")
      (insert "* root :elfeed:\n"))))

;; Lectura de los feeds
(setq-default fill-column 120)
(setq visual-fill-column-width 120
      visual-fill-column-center-text t)
(add-hook 'elfeed-show-mode-hook #'visual-fill-column-mode)

;; -------------------------------
;; Configuración de pdfs
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
;; Configuración de Emms
;; -------------------------------

(emms-all)
(emms-default-players)
(emms-mode-line-mode 1)
(emms-playing-time-mode 1)

(setq emms-source-file-default-directory "~/Music"
      emms-browser-covers #'emms-browser-cache-thumbnail-async
      emms-browser-thumbnail-small-size 64
      emms-browser-thumbnail-medium-size 128
      emms-playlist-buffer-name "*Music*"
      emms-info-asynchronously t
      emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find)

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

;; -------------------------------
;; Configuración de deft
;; -------------------------------

;; Configurar directorio de notas
(setq deft-directory "~/Org/")  ; Ruta donde se guardan las notas

;; Buscamos en subdirectorios
(setq deft-recursive t)

;; Usar extensión .org por defecto
(setq deft-extensions '("org" "md" "txt"))  ; Formatos permitidos
(setq deft-default-extension "org")         ; Extensión predeterminada

;; Ignorar ciertos archivos (ej: archivos temporales de Org)
(setq deft-ignore-file-regexp "\\.#\\|~$")

;; -------------------------------
;; Configuración de readers
;; -------------------------------

(setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package! calibredb
  :commands calibredb
  :config
  (setq calibredb-root-dir "~/Calibre Library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/Calibre Library"))
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
;; Configuración de notificaciones
;; -------------------------------
(after! alert
  (defun my/alert-sound-wrapper (orig-fn message &rest args)
    (apply orig-fn message args)
    (let* ((severity (plist-get args :severity))
           (sound (pcase severity
                    ('high "/usr/share/sounds/freedesktop/stereo/alarm-clock-elapsed.oga")
                    ('moderate "/usr/share/sounds/freedesktop/stereo/message.oga")
                    ('normal "/usr/share/sounds/freedesktop/stereo/dialog-information.oga")
                    ('low "/usr/share/sounds/freedesktop/stereo/complete.oga")
                    (_ "/usr/share/sounds/freedesktop/stereo/dialog-information.oga")))) ; valor por defecto
      (ignore-errors
        (start-process "alert-sound" nil "paplay" sound))))

  (advice-add 'alert :around #'my/alert-sound-wrapper))

(use-package! alert
  :defer t
  :custom
  (alert-default-style (if IS-LINUX 'libnotify 'osx-notifier)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (alert "Welcome, Master" :title "Doom Emacs Awaits")))

(defun my/org-export-pdf-alert (&rest _args)
  (alert "Exportación de Org a PDF completada" :title "Org Export" :severity 'normal))

(advice-add 'org-latex-export-to-pdf :after #'my/org-export-pdf-alert)

(defun my/latex-compilation-alert (filename)
  (alert "Compilación de LaTeX a PDF completada" :title "Latex Compile" :severity 'normal))

(add-hook 'TeX-after-compilation-finished-functions #'my/latex-compilation-alert)
