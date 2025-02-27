;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; -------------------------------
;; Configuración visual y comportamiento
;; -------------------------------

;; Tema y fuentes
(setq doom-theme 'doom-nord)                  ; Tema Nord (oscuro)
(use-package! doom-nord-theme
  :defer t
  :custom
  (doom-nord-brighter-modeline t)
  (doom-nord-padded-modeline t)
  (doom-nord-region-highlight 'frost))

(setq display-line-numbers-type 'relative)    ; Números de línea relativos
(add-hook 'prog-mode-hook #'hl-line-mode)

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

(after! highlight-indent-guides
  (setq highlight-indent-guides-method (if (display-graphic-p)
                                           'bitmap
                                         'character)
        highlight-indent-guides-responsive 'top))

(add-transient-hook! 'doom-first-input-hook
  (let ((cell (assoc 'side
                     (assoc "^\\*\\(?:Wo\\)?Man " display-buffer-alist))))
    (setcdr cell 'right)))

(use-package! doom-modeline
  :defer t
  :custom
  (doom-modeline-modal-icon nil))

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
;; Configuración de magit
;; -------------------------------

(use-package! magit-delta
  :custom (magit-delta-default-dark-theme "Nord")
  :hook   (magit-mode . magit-delta-mode))

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

;; Org-mode
(setq org-directory "~/Org"                   ; Directorio de notas
      org-agenda-files '("~/Org/agenda")
      org-roam-directory "~/Org/notes"
      org-startup-indented t                  ; Indentación automática
      org-ellipsis " ⤵")                     ; Icono para folds

(after! org
  (setq-hook! org-mode
    display-line-numbers nil))

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

;; -------------------------------
;; Configuración de Typescript (NestJS)
;; -------------------------------

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

(setq lsp-dockerfile-language-server-path "docker-langserver")

(add-to-list 'auto-mode-alist
             '("docker-compose.*\\.yml\\'" . docker-compose-mode))

;; -------------------------------
;; Configuración de dired 
;; -------------------------------

(add-hook! dired-mode #'dired-hide-details-mode)
(use-package! dired-open
  :after dired
  :custom
  (dired-open-functions (list #'dired-open-guess-shell-alist
                              #'dired-open-by-extension
                              #'dired-open-subdir))
  (dired-guess-shell-alist-user '(("\\.\\(?:docx\\|djvu\\|eps\\)\\'" "xdg-open")
                                  ("\\.\\(?:\\|gif\\|xpm\\)\\'" "xdg-open")
                                  ("\\.\\(?:xcf\\)\\'" "xdg-open")
                                  ("\\.csv\\'" "xdg-open")
                                  ("\\.tex\\'" "xdg-open")
                                  ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\|mov\\)\\(?:\\.part\\)?\\'" "xdg-open")
                                  ("\\.\\(?:mp3\\|flac\\)\\'" "xdg-open")
                                  ("\\.html?\\'" "xdg-open")
                                  ("\\.md\\'" "xdg-open"))))

(use-package! dired-subtree
  :after dired)

(add-hook! dired-mode #'dired-async-mode)

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
;; Configuración Dotenv 
;; -------------------------------

(use-package! dotenv-mode
  :mode ("\\.env\\.?.*\\'" . dotenv-mode))

;; -------------------------------
;; Configuración en pruebas 
;; -------------------------------

(use-package! alert
  :defer t
  :custom
  (alert-default-style (if IS-LINUX 'libnotify 'osx-notifier)))

(use-package! annotate
  :commands (annotate-load-annotation-data))

(add-hook! find-file
  (let ((file-name (buffer-file-name))
        (annotation-files (mapcar #'car (annotate-load-annotation-data t))))
    (when (and file-name
               (member file-name annotation-files))
      (annotate-mode +1))))

(after! annotate
  (setq annotate-file (expand-file-name "annotate" doom-cache-dir)))

(setq annotate-blacklist-major-mode '(org-mode))

(after! annotate
  (setq annotate-mode-map (make-sparse-keymap))
  (map! :map annotate-mode-map
        :leader
        :prefix ("b a" . "annotate")
        "a" #'annotate-annotate
        "d" #'annotate-delete-annotation
        "s" #'annotate-show-annotation-summary
        "]" #'annotate-goto-next-annotation
        "[" #'annotate-goto-previous-annotation))

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

(use-package! cus-edit
  :defer t
  :custom
  (custom-unlispify-menu-entries nil)
  (custom-unlispify-tag-names nil)
  (custom-unlispify-remove-prefixes nil))

(use-package! proced
  :defer t
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 1))

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
