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

;; Tabs estilo IDE (Centaur Tabs)
(setq centaur-tabs-style "alternate"
      centaur-tabs-height 32
      centaur-tabs-set-bar 'under)

(centaur-tabs-mode t)

;; Treemacs (Árbol de directorios)
(setq treemacs-width 42
      treemacs-follow-mode t                 ; Sigue el archivo activo
      treemacs-indentation 2
      treemacs-git-mode 'extended
      treemacs-use-icons-file-mode t)

;; Folding para lenguajes sin soporte nativo
(add-hook 'sh-mode-hook #'outline-minor-mode) ; Para Shell Script
(add-hook 'markdown-mode-hook #'outline-minor-mode)

(setq hs-isearch-open t           ; Expandir folds al buscar
      doom-modeline-icon t)       ; Mostrar íconos en la barra de estado

;; -------------------------------
;; Configuración de LSP para todos los lenguajes
;; -------------------------------

(setq comment-tabs t)  ; Alinea comentarios con tabs o espacios

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

;; Lua (lua-language-server)
(setq lsp-lua-language-server-install-dir "~/.local/bin/lua-language-server")

;; Shell Script (bashls)
(setq lsp-bash-language-server-path "~/.npm/bin/bash-language-server")

;; Usar el entorno virtual para Python
(setq lsp-pyright-venv-path "~/.emacs-lsp-venv")
(setq python-shell-interpreter "~/.emacs-lsp-venv/bin/python3")

;; Python (Black)
(setq format-all-formatters '(("Python" black)))

;; JavaScript/TypeScript (Prettier)
(setq prettier-js-args '("--trailing-comma" "all"
                         "--single-quote" "true"))

;; C/C++ (clang-format)
(setq clang-format-style "Google")  ; Estilo: Google, LLVM, WebKit, etc.

;; Shell (shfmt)
(setq shfmt-arguments "-i=2"  ; Sangría de 2 espacios
      shfmt-path "~/.go/bin/shfmt")

;; Lua (stylua)
(setq format-all-formatters '(("Lua" stylua)))

;; Lisp (indentación estándar de Emacs)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)  ; Soporte de documentación

;; Activar formateo automático al guardar
(setq +format-on-save-enabled-modes
      '(python-mode js-mode typescript-mode dart-mode c-mode c++-mode sh-mode lua-mode))

;; -------------------------------
;; Configuración de org-mode
;; -------------------------------

;; Org-mode
(setq org-directory "~/Org"                   ; Directorio de notas
      org-agenda-files '("~/Org/agenda.org")
      org-roam-directory "~/Org/notes"
      org-startup-indented t                  ; Indentación automática
      org-ellipsis " ⤵")                     ; Icono para folds

;; Org-mode (Ajustar sangría)
(setq org-edit-src-content-indentation 2)

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

;; Configuración adicional para Org-roam (opcional)
(setq org-roam-capture-templates
      '(("d" "Nota Default" plain "%?"
         :target (file+head "${slug}.org"
                            "${title}\n\n")
         :unnarrowed t
         :mkdir t)))

;; -------------------------------
;; Configuración de Typescript (NestJS)
;; -------------------------------

(use-package! typescript-mode
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq lsp-clients-typescript-auto-install-server t  ; Instalar automáticamente
        lsp-clients-typescript-log-verbosity "debug"  ; Logs detallados
        lsp-completion-enable-auto-import t           ; Auto-imports
        lsp-tsserver-plugins-path "~/node_modules"    ; Ruta de plugins de TS
        ))

(setq lsp-clients-typescript-init-opts '(:plugins ["@nestjs/swagger-plugin"]  
                                         :compilerOptions {:experimentalDecorators t}))

(setq prettier-js-args '("--trailing-comma" "all"
                         "--single-quote" "true")
      eslintd-fix-mode t)  ; Corregir errores al guardar

;; -------------------------------
;; Configuración Docker
;; -------------------------------

(setq lsp-dockerfile-language-server-path "docker-langserver")

(add-to-list 'auto-mode-alist
             '("docker-compose.*\\.yml\\'" . docker-compose-mode))
