;;; lsp.el --- LSP and Tree-sitter configuration  -*- lexical-binding: t; -*-

(defvar +my/lsp-servers-to-install
  '(;; Python
    (python-mode . ("pylsp" "pyright"))
    ;; JavaScript/TypeScript
    (js-mode . ("typescript-language-server" "typescript"))
    (typescript-mode . ("typescript-language-server" "typescript"))
    ;; Go
    (go-mode . ("gopls" "gofmt"))
    ;; Rust
    (rust-mode . ("rust-analyzer" "rustfmt"))
    ;; JSON
    (json-mode . ("vscode-json-languageserver"))
    ;; YAML
    (yaml-mode . ("yaml-language-server"))
    ;; Web (Astro, Svelte)
    (web-mode . ("astro-ls" "svelte-language-server" "typescript-language-server"))
    ;; Nix
    (nix-mode . ("nil"))
    ;; Bash/Shell
    (sh-mode . ("bash-language-server"))
    ;; Docker
    (dockerfile-mode . ("docker-langserver"))
    ;; LaTeX
    (tex-mode . ("texlab"))
    ;; Markdown
    (markdown-mode . ("marksman")))
  "List of modes and their corresponding LSP servers.")

(defvar +my/treesit-grammars-to-install
  '(python javascript typescript go rust json yaml markdown php html css toml)
  "List of tree-sitter grammars to install.")

(defun +my/get-all-lsp-servers ()
  "Returns list of all configured LSP servers."
  (cl-loop for entry in +my/lsp-servers-to-install
           append (cdr entry)))

(defun +my/install-lsp-for-current-mode ()
  "Install LSP server for current mode."
  (interactive)
  (let* ((mode major-mode)
         (servers (cdr (assoc mode +my/lsp-servers-to-install))))
    (if servers
        (dolist (server servers)
          (message "LSP server %s: %s"
                   (if (executable-find server) "installed" "not found")))
      (message "No LSP server configured for %s" mode))))

(defun +my/install-all-lsp-servers ()
  "Install all configured LSP servers."
  (interactive)
  (let ((servers (+my/get-all-lsp-servers)))
    (message "Configured LSP servers: %s" (string-join servers ", "))
    (dolist (server servers)
      (unless (executable-find server)
        (message "[!] %s not found - install manually" server)))
    (message "Check missing servers with SPC l l")))

(defun +my/list-lsp-servers ()
  "List installed and active LSP servers."
  (interactive)
  (let ((buffer (get-buffer-create "*LSP Servers*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Installed LSP servers:\n\n")
      (let ((servers (delete-dups (+my/get-all-lsp-servers))))
        (dolist (server servers)
          (insert (format "[%s] %s\n"
                         (if (executable-find server) "x" "-")
                         server))))
      (insert "\nActive LSP session:\n\n")
      (if (and (featurep 'lsp-mode) (boundp 'lsp-session) lsp-session)
          (insert "Active session detected. Use M-x lsp-describe-session for details.\n")
        (insert "No active LSP session.\n"))
      (insert "\nConfiguration by mode:\n")
      (dolist (entry +my/lsp-servers-to-install)
        (insert (format "  %s: %s\n"
                       (car entry)
                       (string-join (cdr entry) ", ")))))
    (pop-to-buffer buffer)))

(defun +my/install-all-treesit-grammars ()
  "Install all configured tree-sitter grammars."
  (interactive)
  (message "Installing tree-sitter grammars...")
  (dolist (lang +my/treesit-grammars-to-install)
    (treesit-install-language-grammar lang))
  (message "Tree-sitter grammar installation started."))

(defun +my/list-treesit-grammars ()
  "List installed tree-sitter grammars."
  (interactive)
  (let ((buffer (get-buffer-create "*Tree-sitter Grammars*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Tree-sitter grammars:\n\n")
      (insert "Configured grammars:\n")
      (dolist (lang +my/treesit-grammars-to-install)
        (insert (format "  - %s\n" lang)))
      (insert "\nInstallation status:\n")
      (dolist (lang +my/treesit-grammars-to-install)
        (let ((installed (treesit-language-available-p lang)))
          (insert (format "[%s] %s\n"
                         (if installed "x" "-")
                          lang)))))
    (pop-to-buffer buffer)))

(defun +my/show-lsp-status ()
  "Show LSP and tree-sitter status in minibuffer."
  (interactive)
  (let ((lsp-active (if (and (boundp 'lsp-mode) lsp-mode) "Active" "Inactive"))
        (ts-active (if (and (boundp 'treesit-mode) treesit-mode) "Active" "Inactive")))
    (message "LSP: %s | Tree-sitter: %s" lsp-active ts-active)))

(defun +my/lsp-treesit-doctor ()
  "Check LSP and tree-sitter health status."
  (interactive)
  (let ((buffer (get-buffer-create "*LSP & Tree-sitter Doctor*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Doctor: LSP and Tree-sitter Status\n\n")

      ;; Tree-sitter status
      (insert "Tree-sitter:\n\n")
      (dolist (lang +my/treesit-grammars-to-install)
        (let ((available (treesit-language-available-p lang)))
          (insert (format "[%s] %s\n"
                         (if available "x" "-")
                          lang))))

      ;; LSP status
      (insert "\nLSP servers:\n\n")
      (let ((servers (delete-dups (+my/get-all-lsp-servers))))
        (dolist (server servers)
          (insert (format "[%s] %s\n"
                         (if (executable-find server) "x" "-")
                          server))))

      ;; Recommendations
      (insert "\nRecommendations:\n\n")
      (let ((missing-ts (cl-loop for lang in +my/treesit-grammars-to-install
                                 unless (treesit-language-available-p lang)
                                 collect lang))
            (missing-lsp (cl-loop for server in (delete-dups (+my/get-all-lsp-servers))
                                  unless (executable-find server)
                                  collect server)))
        (if missing-ts
            (insert "Missing tree-sitter grammars:\n"
                    (mapconcat (lambda (x) (format "  M-x treesit-install-language-grammar RET %s" x))
                               missing-ts "\n") "\n")
          (insert "All tree-sitter grammars installed [x]\n"))
        (if missing-lsp
            (insert "\nMissing LSP servers (install with npm/pip/etc):\n"
                    (mapconcat (lambda (x) (format "  - %s" x)) missing-lsp "\n"))
          (insert "\nAll LSP servers in PATH [x]\n"))))
    (pop-to-buffer buffer)))

(defun +my/lsp-auto-install-on-find-file-h ()
  "Install LSP server automatically when opening a file."
  (when (and (boundp 'lsp-mode) lsp-mode
             (not (lsp-session)))
    (let* ((mode major-mode)
           (servers (cdr (assoc mode +my/lsp-servers-to-install))))
      (when servers
        (dolist (server servers)
          (unless (executable-find server)
            (message "[!] LSP server %s not found. Install with 'SPC l i'" server))))))

(add-hook 'find-file-hook #'+my/lsp-auto-install-on-find-file-h)

(after! lsp-mode
  (setq lsp-idle-delay 0.3
        lsp-log-io nil
        lsp-completion-provider :capf
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil
        lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-links nil
        lsp-warn-no-matched-clients nil

        lsp-gopls-completeUnimported t
        lsp-gopls-staticcheck t
        lsp-gopls-analyses '((unusedparams . t)
                             (unusedwrite . t)))

        lsp-headerline-breadcrumb-enable t)

  (defun my/find-tsdk ()
    "Find TypeScript tsdk path dynamically."
    (or (when (executable-find "tsc")
          (let ((tsc-dir (file-name-directory (directory-file-name (file-name-directory (executable-find "tsc"))))))
            (when tsc-dir
              (expand-file-name "lib/node_modules/typescript/lib" tsc-dir))))
        (when (executable-find "typescript-language-server")
          (let* ((dir (file-name-directory (executable-find "typescript-language-server")))
                 (node-modules (expand-file-name "../../lib/node_modules" dir)))
            (when (file-directory-p node-modules)
              (expand-file-name "typescript/lib" node-modules))))))

  (when (executable-find "astro-ls")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "astro-ls")
                     :server-id 'astro-ls
                     :major-modes '(web-mode)
                     :activation-fn (lambda (filename _mode)
                                     (string-match-p "\\.astro\\'" filename)))))

  (when (executable-find "svelte-language-server")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "svelte-language-server")
                     :server-id 'svelte-ls
                     :major-modes '(web-mode)
                     :activation-fn (lambda (filename _mode)
                                     (string-match-p "\\.svelte\\'" filename))))))

(after! lsp-ui
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 72
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.5
        lsp-ui-sideline-enable nil
        lsp-ui-peek-enable t))

(setq lsp-enable-suggest-server-download t
      lsp-auto-execute-action t
      lsp-completion-enable t)

(setq company-minimum-prefix-length 2
      company-idle-delay 0.1
      lsp-pyright-multi-root t)

(use-package! tide
  :hook ((js-mode . tide-setup)
         (typescript-mode . tide-setup)))

(setq lsp-clients-clangd-executable "/usr/bin/clangd")
(setq clang-format-style "Google")

(setq lsp-bash-language-server-path "~/.volta/bin/bash-language-server")

(setq lsp-pyright-venv-path "~/.emacs-lsp-venv")
(setq python-shell-interpreter "~/.emacs-lsp-venv/bin/python3")

(setq format-all-formatters '(("Python" black)))

(setq shfmt-arguments "-i=2"
      shfmt-path "~/.go/bin/shfmt")

(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(setq +format-on-save-enabled-modes
      '(python-mode js-mode typescript-mode c-mode c++-mode sh-mode lua-mode))

(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'yaml-mode-hook #'lsp-deferred)
(add-hook 'json-mode-hook #'lsp-deferred)
(add-hook 'web-mode-hook #'lsp-deferred)

(defun install-lsp-servers ()
  "Install all required LSP servers"
  (interactive)

  (unless (file-exists-p "~/.emacs-lsp-venv/bin/pylsp")
    (shell-command "python3 -m venv ~/.emacs-lsp-venv")
    (shell-command "~/.emacs-lsp-venv/bin/pip install python-lsp-server pyright pylint black"))

  (unless (executable-find "typescript-language-server")
    (shell-command "npm install -g typescript-language-server vscode-langservers-extracted typescript"))

  (unless (executable-find "tsc")
    (shell-command "npm install -g typescript"))

  (unless (executable-find "docker-langserver")
    (shell-command "npm install -g dockerfile-language-server-nodejs"))

  (unless (executable-find "yaml-language-server")
    (shell-command "npm install -g yaml-language-server"))

  (unless (executable-find "bash-language-server")
    (shell-command "npm install -g bash-language-server"))

  (unless (executable-find "astro-ls")
    (shell-command "npm install -g @astrojs/language-server"))

  (unless (executable-find "svelte-language-server")
    (shell-command "npm install -g svelte-language-server")))

(add-hook 'doom-after-init-hook
          (defun run-install-lsp-servers-once ()
            (unless (file-exists-p (expand-file-name ".lsp-installed" doom-cache-dir))
              (install-lsp-servers)
              (write-region "" nil (expand-file-name ".lsp-installed" doom-cache-dir)))))

(after! treesit
  (setq treesit-auto-install t
        treesit-font-lock-level 4))

(map! :leader
      (:prefix ("l" . "lsp")
       :desc "Install LSP for current mode" "+" #'+my/install-lsp-for-current-mode
       :desc "Install all LSP servers"     "i" #'+my/install-all-lsp-servers
       :desc "List LSP servers"            "l" #'+my/list-lsp-servers
       :desc "Show LSP status"             "s" #'+my/show-lsp-status
       :desc "Doctor (check health)"       "d" #'+my/lsp-treesit-doctor

       (:prefix ("t" . "tree-sitter")
         :desc "Install all grammars"       "i" #'+my/install-all-treesit-grammars
         :desc "List grammars"              "l" #'+my/list-treesit-grammars
         :desc "Ensure current installed"   "e" #'treesit-ensure-installed)))

(provide '+my-lsp)
;;; lsp.el ends here
