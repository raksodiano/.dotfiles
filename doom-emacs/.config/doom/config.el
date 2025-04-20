;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Configuración visual y comportamiento
;; -------------------------------

;; Tema y fuentes
(setq doom-theme 'doom-nord)                  ; Tema Nord (oscuro)
(setq display-line-numbers-type 'relative)    ; Números de línea relativos
(add-hook 'prog-mode-hook #'hl-line-mode)

;; Iniciar maximizado
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Configuración global para usar 2 espacios en la tabulación y la sangría
(setq-default tab-width 2)                ;; Configura el tamaño de la tabulación a 2 espacios
(setq-default indent-tabs-mode t)
(setq-default sh-basic-offset 2)          ;; Configura la sangría para scripts shell (sh, bash) a 2 espacios
(setq-default sh-indentation 2)
(setq-default fish-indent-offset 2)       ;; Configura la sangría para Fish shell a 2 espacios
(setq-default conf-basic-offset 2)        ;; Configura la sangría para archivos de configuración a 2 espacios

(after! sh-script
    (setq sh-basic-offset 2)
  (setq tab-width 2)
  (setq indent-tabs-mode t))

(after! fish-mode
    (setq fish-indent-offset 2)
  (setq tab-width 2)
  (setq indent-tabs-mode t))

(use-package! doom-nord-theme
  :defer t
  :custom
  (doom-nord-brighter-modeline t)
  (doom-nord-padded-modeline t)
  (doom-nord-region-highlight 'frost))

;; Modo zen
(after! writeroom-mode
    (setq +zen-text-scale 1.25))

(custom-set-faces!
    '(mode-line :height 90 :inherit 'variable-pitch)
    '(mode-line-inactive :height 80 :inherit 'variable-pitch))

(after! all-the-icons
    (setq all-the-icons-scale-factor 1.1))

(after! doom-modeline
    (setq doom-modeline-buffer-file-name-style 'truncate-with-project))

;; Tabs estilo IDE (Centaur Tabs)
(setq centaur-tabs-style "alternate"
      centaur-tabs-height 32
      centaur-tabs-set-bar 'under)

(centaur-tabs-mode t)

;; Folding para lenguajes sin soporte nativo
(add-hook 'sh-mode-hook #'outline-minor-mode) ; Para Shell Script
(add-hook 'markdown-mode-hook #'outline-minor-mode)

(setq hs-isearch-open t           ; Expandir folds al buscar
      doom-modeline-icon t)       ; Mostrar íconos en la barra de estado

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

;; -------------------------------
;; Configuración de directorios
;; -------------------------------

(setq my/directories '("~/Org"
                       "~/Org/rss"
                       "~/Org/agenda"
                       "~/Org/notes"
                       "~/Org/notes-work"
                       "~/Workspace"
                       "~/Workspace/books"
                       "~/Workspace/blog"
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
;; Configuración de notificaciones
;; -------------------------------

(use-package! alert
  :defer t
  :custom
  (alert-default-style (if IS-LINUX 'libnotify 'osx-notifier)))

;; -------------------------------
;; Configuración de escritura
;; -------------------------------

(use-package! lorem-ipsum
  :commands (lorem-ipsum-insert-paragraphs
             lorem-ipsum-insert-sentences
             lorem-ipsum-insert-list))

(after! flyspell
    (setq flyspell-lazy-idle-seconds 1.5))

;; -------------------------------
;; Configuración de LSP para todos los lenguajes
;; -------------------------------

;; LSP general
(setq lsp-ui-sideline-enable t                ; Información en barra lateral
      lsp-ui-doc-enable t                     ; Documentación flotante
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
      '(python-mode js-mode typescript-mode dart-mode c-mode c++-mode sh-mode lua-mode))

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

(add-hook 'magit-post-push-hook 'magit-refresh)
(setq magit-blame-heading-format "%-20a %C %s\n") ; Formato de autor/fecha
(custom-set-faces
 '(magit-blame-hash ((t (:foreground "#7F7F7F"))))) ; Color del hash

;; (use-package! magit-delta
;;   :custom (magit-delta-default-dark-theme "Nord")
;;   :hook   (magit-mode . magit-delta-mode))

;; (after! magit-delta
;;   (defcustom dan/magit-delta-point-max 50000
;;     "Maximum length of diff buffer which `magit-delta' will tolerate."
;;     :group 'magit-delta
;;     :type  'natnum)
;;   (defadvice! dan/magit-delta-colorize-maybe (fn &rest args)
;;     "Disable mode if there are too many characters."
;;     :around #'magit-delta-call-delta-and-convert-ansi-escape-sequences
;;     (if (<= (point-max) dan/magit-delta-point-max)
;;         (apply fn args)
;;       (magit-delta-mode -1))))

;; (after! magit
;;   (add-hook! 'magit-post-refresh-hook
;;     (when (and (not magit-delta-mode)
;;                (<= (point-max) dan/magit-delta-point-max))
;;       (magit-delta-mode +1))))

;; -------------------------------
;; Configuración de org-mode
;; -------------------------------

(setq org-directory "~/Org"                   ; Directorio de notas
      org-agenda-files '("~/Org/agenda")
      org-roam-directory "~/Org/notes"
      org-startup-indented t                  ; Indentación automática
      org-ellipsis " ⤵")                     ; Icono para folds

;; Ajustar sangría
(setq org-edit-src-content-indentation 2)

(after! org
    (setq-default fill-column 120)
  (setq visual-fill-column-width 120
        visual-fill-column-center-text t)
  (add-hook 'org-mode-hook #'visual-fill-column-mode))

(after! org
    (setq-hook! org-mode
      display-line-numbers nil))

(after! org
    (custom-declare-face '+org-todo-wait  '((t (:inherit (bold mode-line-emphasis org-todo)))) "")
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAIT(w)" "IDEA(i)" "EVENT(e)" "|"
                             "DONE(d)" "CANCELLED(c)"))
        org-todo-keyword-faces '(("NEXT"      . +org-todo-active)
                                 ("WAIT"      . +org-todo-wait)
                                 ("EVENT"     . +org-todo-onhold)
                                 ("PROJ"      . +org-todo-project)
                                 ("CANCELLED" . +org-todo-cancel))))

;; Plantillas de captura con categorías
(setq org-capture-templates
      '(("t" "Tarea" entry
         (file+datetree "~/Org/notes/task/tareas.org" "Tareas Pendientes")
         "* TODO %?\nFecha: %T\n%i\n%a"
         :mkdir t  ; Crea la carpeta si no existe
         :empty-lines 1)

        ("n" "Nota General" entry
         (file+headline "~/Org/notes/note/notas.org" "Notas")
         "* %?\nFecha: %T\n%i\n%a"
         :mkdir t)

        ("h" "Nota Hugo (Blog)" entry
         (file+olp "~/Org/notes/hugo/posts.org" "Borradores")
         "* %?\nFecha: %T\n%i\n%a"
         :mkdir t)))

;; Configuración adicional para Org-roam
(setq org-roam-capture-templates
      '(("d" "Nota Default" plain "%?"
         :target (file+head "${slug}.org"
                  "${title}\n\n")
         :unnarrowed t
         :mkdir t)))

;; -------------------------------
;; Configuración de publish (para blogging)
;; -------------------------------

(setq org-publish-project-alist
      `(
        ("blog-posts"
         :base-directory "~/Workspace/blog/posts/"
         :base-extension "org"
         :publishing-directory "~/Workspace/blog/exports/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :html-preamble ,(with-temp-buffer
                           (insert-file-contents "~/Workspace/blog/header.org")
                           (buffer-string))
         :html-postamble ,(with-temp-buffer
                            (insert-file-contents "~/Workspace/blog/footer.org")
                            (buffer-string)))

        ("blog-assets"
         :base-directory "~/Workspace/blog/assets/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|woff\\|woff2"
         :publishing-directory "~/Workspace/blog/exports/assets/"
         :recursive t
         :publishing-function org-publish-attachment)

        ("blog" :components ("blog-posts" "blog-assets"))))

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
;; Configuración Docker
;; -------------------------------

(add-to-list 'auto-mode-alist
             '("docker-compose[.-]?\\(yml\\|yaml\\)\\'" . yaml-mode))

;; (setq lsp-dockerfile-language-server-path "docker-langserver")

;; (use-package! docker-compose-mode
;;   :mode ("docker-compose.yml\\'" . docker-compose-mode)
;;   :config
;;   (setq docker-compose-command "docker-compose"))

;; -------------------------------
;; Configuración de dired
;; -------------------------------

(add-hook! dired-mode #'dired-hide-details-mode)
(add-hook! dired-mode #'dired-async-mode)

(use-package! dired-subtree
  :after dired)

(after! dired
    (map! :map dired-mode-map
     :n "R" #'dired-async-do-rename
     :n "C" #'dired-async-do-copy
     :n "S" #'dired-async-do-symlink
     :n "H" #'dired-async-do-hardlink))

(after! dired
    (setq dired-dwim-target #'dired-dwim-target-recent))

(after! dirvish
    (setq dirvish-default-layout '(0 0 0.4)
     dirvish-layout-recipes '((1 0.11 0.55)
                              (0 0    0.40))))

(after! dirvish
    (pushnew! dirvish-attributes 'file-size))

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

(use-package! cape
  :init
  (after! term
      (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))
  :defer t)

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
  :config
  (setq detached-notification-function (if IS-LINUX
                                           #'detached-state-transition-notifications-message
                                         #'detached-extra-alert-notification)
        detached-db-directory doom-cache-dir
        detached-init-block-list   '(dired-rsync dired)
        detached-session-directory (temporary-file-directory)))

(map! :g "M-&" #'detached-shell-command)

(use-package! detached-consult
  :defer t
  :init
  (map! :leader
        :desc "Detached Sessions" :g "o s" #'detached-consult-session))

(use-package! detached-compile
  :defer t
  :init
  (map! :leader
        :desc "Compile" :g  "c c" #'detached-compile
        :desc "Recompile" :g "c C" #'detached-compile-recompile))

(use-package! detached-list
  :defer t
  :init
  (map! :leader
        :desc "Detached Manage Sessions" :g "o S" #'detached-list-sessions)
  :config
  (evil-set-initial-state 'detached-list-mode 'emacs))

(after! detached
    (detached-init))

(use-package! vlf
  :defer-incrementally t
  :custom
  (vlf-batch-size-remote read-process-output-max)
  :config
  (require 'vlf-setup)
  (add-hook! 'vlf-mode-hook #'so-long-mode))

(after! so-long
    (add-to-list 'so-long-mode-preserved-variables 'vlf-mode))

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

(use-package! emms
  :config
  (setq emms-player-list '(emms-player-mpv)))

(require 'emms-setup)
(emms-all)
(emms-default-players)

(require 'emms-browser)
(setq emms-browser-default-directory "~/Music/")
(setq emms-browser-depth nil)  ; Sin límite de profundidad
(setq emms-browser-recursive t)

(map! :leader
      :prefix ("m" . "Music")
      "b" #'emms-smart-browser
      "p" #'emms-playlist-mode-go
      "SPC" #'emms-pause
      "n" #'emms-next
      "s" #'emms-stop
      "f" #'emms-play-find
      "r" #'emms-random)

;; -------------------------------
;; Configuración de Workspaces
;; -------------------------------

(defun my/new-frame-with-workspace ()
  "Crea un nuevo frame y un nuevo workspace vinculado a él."
  (interactive)
  (let ((new-frame (make-frame)))
    (select-frame-set-input-focus new-frame)
    (doom/workspace-new nil t) ; Crea un workspace nuevo
    (message "Nuevo frame + workspace creado!")))

(map! :leader
      :prefix "TAB"
      :desc "Nuevo frame + workspace" "N" #'my/new-frame-with-workspace)

;; -------------------------------
;; Configuración de deft
;; -------------------------------

;; Configurar directorio de notas
(setq deft-directory "~/Org/")  ; Ruta donde se guardan las notas

;; Usar extensión .org por defecto
(setq deft-extensions '("org" "md" "txt"))  ; Formatos permitidos
(setq deft-default-extension "org")         ; Extensión predeterminada

;; Ignorar ciertos archivos (ej: archivos temporales de Org)
(setq deft-ignore-file-regexp "\\.#\\|~$")
