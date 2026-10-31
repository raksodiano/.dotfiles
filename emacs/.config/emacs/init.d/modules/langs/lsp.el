;;; lsp.el -*- lexical-binding: t; -*-
;;; lsp.el --- Language servers: installation  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from `install-lsp-servers' in Doom's lisp/lsp.el, which used to
;; run from doom-after-init-hook on first start. It is an interactive
;; command here so that nothing shells out during Emacs startup.
;;
;; The list of servers mirrors Doom's `+my/lsp-servers-to-install'.
;; Usage:
;;   M-x my/install-lsp-servers        install what is missing
;;   M-x my/install-lsp-servers C-u    only report, install nothing

;;; Code:

(require 'seq)

;;; ### Toolchain paths

;; Volta (and a conventional ~/.npm-global) keep user binaries outside the
;; default exec-path, so neither `executable-find' nor lsp-mode sees them.
;; lsp-mode itself also needs them there to start servers.
(dolist (dir '("~/.volta/bin" "~/.npm-global/bin" "/usr/local/bin"))
  (let ((full (expand-file-name dir)))
    (when (and (file-directory-p full) (not (member full exec-path)))
      (add-to-list 'exec-path full))))

;;; ### Servers

(defconst my/lsp-venv
  (or (getenv "EMACS_LSP_VENV") (expand-file-name "~/.emacs-lsp-venv"))
  "Virtual environment holding the Python language servers.")

(defconst my/lsp-npm-servers
  '(("typescript-language-server" . "typescript-language-server vscode-langservers-extracted typescript")
    ("tsc"                        . "typescript")
    ("yaml-language-server"       . "yaml-language-server")
    ("bash-language-server"       . "bash-language-server")
    ("docker-langserver"          . "dockerfile-language-server-nodejs")
    ("astro-ls"                   . "@astrojs/language-server")
    ("svelteserver"                . "svelte-language-server")
    ("vue-language-server"        . "@vue/language-server")
    ("intelephense"               . "intelephense")
    ("prettier"                   . "prettier"))
  "Servers installed with npm, as (COMMAND . PACKAGES).

COMMAND is the executable to look for; PACKAGES is the npm package list
passed to `npm install -g'.")

(defconst my/lsp-cargo-servers
  '(("marksman" . "marksman")
    ("texlab"   . "texlab"))
  "Servers installed with cargo, as (COMMAND . PACKAGE).")

(defconst my/lsp-go-servers
  '(("shfmt" . "mvdan.cc/sh/v3/cmd/shfmt@latest"))
  "Servers installed with `go install', as (COMMAND . PACKAGE).")

;;; ### Reporting

(defvar my/lsp--install-log nil
  "List of messages produced by the last `my/install-lsp-servers'.")

(defun my/lsp--log (fmt &rest args)
  "Report a message, keeping it in `my/lsp--install-log'."
  (let ((text (apply #'format fmt args)))
    (push text my/lsp--install-log)
    (message "my/lsp: %s" text)))

(defun my/lsp--have (command)
  "Return non-nil if COMMAND is on `exec-path'."
  (and (executable-find command) t))

;;; ### Installation

(defun my/install-python-servers (check-only)
  "Install the Python language servers into `my/lsp-venv'.

With CHECK-ONLY, only report the status."
  (if (executable-find (expand-file-name "bin/pylsp" my/lsp-venv))
      (my/lsp--log "python: ok (%s)" my/lsp-venv)
    (if check-only
        (my/lsp--log "python: MISSING, run M-x my/install-lsp-servers")
      (condition-case err
          (progn
            (my/lsp--log "python: creating %s" my/lsp-venv)
            (call-process "python3" nil nil nil "-m" "venv" my/lsp-venv)
            (call-process (expand-file-name "bin/pip" my/lsp-venv)
                          nil nil nil "install" "--quiet"
                          "python-lsp-server" "pyright" "pylint" "black")
            (my/lsp--log "python: installed"))
        (error (my/lsp--log "python: FAILED: %s"
                            (error-message-string err)))))))

(defun my/lsp--npm ()
  "Return the npm executable path, looking beyond `exec-path'."
  (or (executable-find "npm")
      (let ((volta (expand-file-name "~/.volta/bin/npm")))
        (and (file-executable-p volta) volta))))

(defun my/install-npm-servers (check-only)
  "Install the npm language servers with `npm install -g'.

With CHECK-ONLY (prefix argument interactively), only report status."
  (interactive "P")
  (cond
   ((not (my/lsp--npm))
    (my/lsp--log "npm: not found, install nodejs/npm first"))
   (t
    (dolist (entry my/lsp-npm-servers)
      (let ((command (car entry))
            (packages (cdr entry)))
        (cond
         ((my/lsp--have command)
          (my/lsp--log "npm: ok (%s)" command))
         (check-only
          (my/lsp--log "npm: MISSING (%s)" command))
         (t
          (condition-case err
              (let ((outbuf (get-buffer-create "*my-lsp-npm*"))
                    status output)
                (with-current-buffer outbuf (erase-buffer))
                (my/lsp--log "npm: installing %s" packages)
                (setq status (apply #'call-process (my/lsp--npm) nil outbuf nil
                                    "install" "-g" (split-string packages)))
                (setq output (with-current-buffer outbuf (buffer-string)))
                (cond
                 ((and (zerop status) (my/lsp--have command))
                  (my/lsp--log "npm: installed %s" command))
                 (t
                  (my/lsp--log "npm: FAILED %s (exit %s): %s" command status
                               (string-trim
                                (mapconcat #'identity
                                           (last (split-string output "\n" t) 3)
                                           " "))))))
            (error (my/lsp--log "npm: FAILED %s: %s"
                                command (error-message-string err)))))))))))

(defun my/install-cargo-servers (check-only)
  "Install the cargo language servers.

With CHECK-ONLY, only report the status."
  (dolist (entry my/lsp-cargo-servers)
    (let ((command (car entry))
          (package (cdr entry)))
      (cond
       ((my/lsp--have command)
        (my/lsp--log "cargo: ok (%s)" command))
       ((not (executable-find "cargo"))
        (my/lsp--log "cargo: not found, skipping %s" command))
       (check-only
        (my/lsp--log "cargo: MISSING (%s)" command))
       (t
        (condition-case err
            (progn
              (my/lsp--log "cargo: installing %s" package)
              (call-process "cargo" nil nil nil "install" package)
              (my/lsp--log "cargo: installed %s" command))
          (error (my/lsp--log "cargo: FAILED %s: %s"
                              command (error-message-string err)))))))))

(defun my/install-go-servers (check-only)
  "Install the Go language servers.

With CHECK-ONLY, only report the status."
  (dolist (entry my/lsp-go-servers)
    (let ((command (car entry))
          (package (cdr entry)))
      (cond
       ((my/lsp--have command)
        (my/lsp--log "go: ok (%s)" command))
       ((not (executable-find "go"))
        (my/lsp--log "go: not found, skipping %s" command))
       (check-only
        (my/lsp--log "go: MISSING (%s)" command))
       (t
        (condition-case err
            (progn
              (my/lsp--log "go: installing %s" package)
              (call-process "go" nil nil nil "install" package)
              (my/lsp--log "go: installed %s" command))
          (error (my/lsp--log "go: FAILED %s: %s"
                              command (error-message-string err)))))))))

;;; ### Entry point

(defun my/install-lsp-servers (&optional check-only)
  "Install the language servers the configuration expects.

With a prefix argument CHECK-ONLY, only report what is missing and
install nothing."
  (interactive "P")
  (setq my/lsp--install-log nil)
  (my/install-python-servers check-only)
  (my/install-npm-servers check-only)
  (my/install-cargo-servers check-only)
  (my/install-go-servers check-only)
  ;; Whatever is already on the system and is not managed by the lists
  ;; above, so the report is complete.
  (dolist (command '("rust-analyzer" "gopls"))
    (when (my/lsp--have command)
      (my/lsp--log "system: ok (%s)" command)))
  (let* ((missing (seq-filter (lambda (line) (string-match-p "MISSING\\|not found\\|FAILED" line))
                              my/lsp--install-log))
         (report (mapconcat #'identity (reverse my/lsp--install-log) "\n"))
         (summary (format "%s — %d of %d entries need attention\n\n%s"
                          (if missing "incomplete" "all present")
                          (length missing)
                          (length my/lsp--install-log)
                          report)))
    (if noninteractive
        (princ (concat "my/lsp: " summary "\n"))
      (let ((buf (get-buffer-create "*my-lsp*")))
        (with-current-buffer buf
          (read-only-mode -1)
          (erase-buffer)
          (insert summary)
          (goto-char (point-min))
          (read-only-mode 1))
        (display-buffer buf)))
    my/lsp--install-log))

;;; ### Client (lsp-mode)

(require 'project)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-idle-delay 0.3)
  (lsp-log-io nil)
  (lsp-completion-provider :capf)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-links nil)
  (lsp-warn-no-matched-clients nil)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-enable-suggest-server-download t)
  (lsp-auto-execute-action t)
  (lsp-completion-enable t)
  (lsp-gopls-completeUnimported t)
  (lsp-gopls-staticcheck t)
  (lsp-pyright-multi-root t)
  (lsp-pyright-venv-path my/lsp-venv)
  (lsp-clients-clangd-executable "/usr/bin/clangd")
  (lsp-bash-language-server-path "~/.volta/bin/bash-language-server")
  :config
  (setq lsp-gopls-analyses '((unusedparams . t)
                            (unusedwrite . t)))
  ;; Language-specific extras: golang staticcheck etc. kept above.
  (when (executable-find "astro-ls")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "astro-ls")
                     :server-id 'astro-ls
                     :major-modes '(web-mode)
                     :activation-fn (lambda (filename _mode)
                                     (string-match-p "\\.astro\\'" filename)))))

  (when (executable-find "svelteserver")
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "svelteserver")
                     :server-id 'svelte-ls
                     :major-modes '(web-mode)
                     :activation-fn (lambda (filename _mode)
                                     (string-match-p "\\.svelte\\'" filename)))))

  (defun my/lsp-find-tsdk ()
    "Find TypeScript tsdk path dynamically."
    (or (when (executable-find "tsc")
          (let ((tsc-dir (file-name-directory
                          (directory-file-name
                           (file-name-directory (executable-find "tsc"))))))
            (when tsc-dir
              (expand-file-name "lib/node_modules/typescript/lib" tsc-dir))))
        (when (executable-find "typescript-language-server")
          (let* ((dir (file-name-directory (executable-find "typescript-language-server")))
                 (node-modules (expand-file-name "../../lib/node_modules" dir)))
            (when (file-directory-p node-modules)
              (expand-file-name "typescript/lib" node-modules))))))

  (setq python-shell-interpreter (expand-file-name "bin/python3" my/lsp-venv)))

(use-package lsp-ui
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-height 8)
  (lsp-ui-doc-max-width 72)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 0.5)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-peek-enable t))

;; Cape (was capefy) - extra completion utilities for lsp's capf.
(use-package cape
  :config
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

;; Activate LSP automatically for the languages Doom enabled.
(dolist (hook '(python-mode-hook python-ts-mode-hook
                js-mode-hook js-ts-mode-hook
                typescript-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook
                go-mode-hook go-ts-mode-hook
                rust-mode-hook rust-ts-mode-hook
                yaml-mode-hook yaml-ts-mode-hook
                json-mode-hook json-ts-mode-hook
                web-mode-hook vue-mode-hook php-mode-hook
                html-mode-hook html-ts-mode-hook
                sh-mode-hook bash-ts-mode-hook
                markdown-mode-hook))
  (add-hook hook #'lsp-deferred))

;; Note: tide is intentionally NOT pulled in (removed per migration plan).

;;; ### Helpers (from Doom's lsp.el)

(defvar my/lsp-servers-to-install
  '(;; Python
    (python-mode . ("pylsp" "pyright"))
    ;; JavaScript/TypeScript
    (js-mode . ("typescript-language-server" "typescript"))
    (js-ts-mode . ("typescript-language-server" "typescript"))
    (typescript-mode . ("typescript-language-server" "typescript"))
    (typescript-ts-mode . ("typescript-language-server" "typescript"))
    (tsx-ts-mode . ("typescript-language-server" "typescript"))
    ;; Go
    (go-mode . ("gopls" "gofmt"))
    (go-ts-mode . ("gopls" "gofmt"))
    ;; Rust
    (rust-mode . ("rust-analyzer" "rustfmt"))
    (rust-ts-mode . ("rust-analyzer" "rustfmt"))
    ;; JSON
    (json-mode . ("vscode-json-languageserver"))
    ;; YAML
    (yaml-mode . ("yaml-language-server"))
    ;; Web
    (web-mode . ("astro-ls" "svelteserver" "typescript-language-server"))
    ;; Vue
    (vue-mode . ("vue-language-server"))
    ;; PHP
    (php-mode . ("intelephense"))
    ;; Bash/Shell
    (sh-mode . ("bash-language-server")))
  "List of modes and their corresponding LSP servers.")

(defun my/get-all-lsp-servers ()
  "Return list of all configured LSP servers."
  (cl-loop for entry in my/lsp-servers-to-install
           append (cdr entry)))

(defun my/install-lsp-for-current-mode ()
  "Install LSP server for current mode."
  (interactive)
  (let* ((mode major-mode)
         (servers (cdr (assoc mode my/lsp-servers-to-install))))
    (if servers
        (dolist (server servers)
          (message "LSP server %s: %s"
                   (if (executable-find server) "installed" "not found")))
      (message "No LSP server configured for %s" mode))))

(defun my/list-lsp-servers ()
  "List configured LSP servers and their status."
  (interactive)
  (let ((buffer (get-buffer-create "*LSP Servers*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Configured LSP servers:\n\n")
      (dolist (server (delete-dups (my/get-all-lsp-servers)))
        (insert (format "[%s] %s\n"
                       (if (executable-find server) "x" "-")
                       server)))
      (insert "\nConfiguration by mode:\n")
      (dolist (entry my/lsp-servers-to-install)
        (insert (format "  %s: %s\n"
                       (car entry)
                       (string-join (cdr entry) ", ")))))
    (pop-to-buffer buffer)))

(defun my/show-lsp-status ()
  "Show LSP status in minibuffer."
  (interactive)
  (message "LSP: %s"
           (if (and (boundp 'lsp-mode) lsp-mode) "Active" "Inactive")))

;; Lsp-treesit doctor merged with the treesit version.
(defun my/lsp-treesit-doctor ()
  "Doctor: LSP and tree-sitter status."
  (interactive)
  (let ((buffer (get-buffer-create "*LSP & Tree-sitter Doctor*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "LSP & Tree-sitter Doctor\n\n")
      (insert "Tree-sitter grammars:\n")
      (dolist (lang my/treesit-grammars-to-install)
        (insert (format "  [%s] %s\n"
                        (if (treesit-language-available-p lang) "x" "-")
                        lang)))
      (insert "\nLSP servers:\n")
      (dolist (server (delete-dups (my/get-all-lsp-servers)))
        (insert (format "  [%s] %s\n"
                        (if (executable-find server) "x" "-")
                        server))))
    (pop-to-buffer buffer)))

;;; ### Leader bindings

(my-leader
 '(("l i"  . my/install-lsp-servers)
   ("l l"  . my/list-lsp-servers)
   ("l s"  . my/show-lsp-status)
   ("l d"  . my/lsp-treesit-doctor)
   ("l +"  . my/install-lsp-for-current-mode)))

(provide 'my-lsp)
;;; lsp.el ends here