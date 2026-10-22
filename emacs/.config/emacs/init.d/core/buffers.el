;;; my-buffers.el -*- lexical-binding: t; -*-
;;; my-buffers.el --- Buffers, files and completion  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's :emacs and :completion modules plus the Dired, consult
;; and vlf sections of config.el.
;;
;; NOTE: consult, consult-buffering, project, dired and flyspell are built
;; into Emacs 31, so they are configured with eval-after-load instead of
;; use-package (which would try to install them from an archive). The
;; :buffer-templates API and consult-buffer-save-window-config no longer
;; exist in current consult. The `capefy' package was folded into `cape';
;; the completion-at-point backends are wired directly instead.

;;; Code:

;;; ### Completion

;; Doom's (company +childframe +tng) becomes Corfu plus Cape. Corfu's popup
;; is now built in, so only the variables that still exist are set.
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  ;; NOTE: corfu-global-matches, corfu-echo-persistent,
  ;; corfu-cycle-on-no-match and corfu-order were removed in recent
  ;; corfu releases; these are the equivalents that still exist.
  (corfu-preview-current 'insert)
  (corfu-quit-no-match nil)
  (corfu-sort-override-function #'ignore)
  (corfu-max-width 120))

;;; ### Vertico (Doom's :completion vertico +icons +childframe)

;; Ported from Doom's modules/completion/vertico/{config,autoload/vertico}.el.
;; Doom macros translated: use-package! -> use-package, map! -> define-key,
;; defadvice! -> defun + advice-add, cmds! -> guarded lambdas, set-popup-rule!
;; -> display-buffer-alist. Doom-only helpers (doom-project-root, better-jumper
;; jump list, Doom package maps, docker sources) are dropped; everything that
;; shapes M-x itself is kept.
(require 'subr-x)

(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command."
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (unless (require 'embark nil t)
      (user-error "Embark not installed, aborting..."))
    (save-selected-window
      (let (embark-quit-after-action)
        (embark-dwim)))))

(defun +vertico/enter-or-preview ()
  "Enter directory or embark preview on current candidate."
  (interactive)
  (when (> 0 vertico--index)
    (user-error "No vertico session is currently active"))
  (if (and (let ((cand (vertico--candidate)))
             (or (string-suffix-p "/" cand)
                 (and (vertico--remote-p cand)
                      (string-suffix-p ":" cand))))
           (not (equal vertico--base ""))
           (eq 'file (vertico--metadata-get 'category)))
      (vertico-insert)
    (condition-case _
        (+vertico/embark-preview)
      (user-error (vertico-directory-enter)))))

(defun +vertico/embark-export-write ()
  "Export the current vertico candidates to a writable buffer."
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (let ((embark-after-export-hook
         `(,@embark-after-export-hook
           ,(pcase-let ((`(,type . _)
                         (run-hook-with-args-until-success 'embark-candidate-collectors)))
              (pcase type
                ('consult-grep #'wgrep-change-to-wgrep-mode)
                ('file #'wdired-change-to-wdired-mode)
                ('consult-location #'occur-edit-mode)
                ('consult-xref (if (fboundp 'xref-change-to-xref-edit-mode)
                                   #'xref-change-to-xref-edit-mode
                                 (user-error "Writable xref export requires Emacs 31+")))
                (x (user-error "embark category %S doesn't support writable export" x)))))))
    (embark-export)))

(defun +vertico/embark-magit-status (file)
  "Run `magit-status' on repo containing the embark target."
  (interactive "GFile: ")
  (magit-status (locate-dominating-file file ".git")))

(defun +vertico-basic-remote-try-completion (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-try-completion string table pred point)))

(defun +vertico-basic-remote-all-completions (string table pred point)
  (and (vertico--remote-p string)
       (completion-basic-all-completions string table pred point)))

(defun +vertico-orderless-dispatch (pattern _index _total)
  "Like `orderless-affix-dispatch', but allows affixes to be escaped."
  (let ((len (length pattern))
        (alist orderless-affix-dispatch-alist))
    (when (> len 0)
      (cond
       ;; Ignore single dispatcher character
       ((and (= len 1) (alist-get (aref pattern 0) alist)) #'ignore)
       ;; Prefix
       ((when-let* ((style (alist-get (aref pattern 0) alist))
                    ((not (char-equal (aref pattern (max (1- len) 1)) ?\\))))
          (cons style (substring pattern 1))))
       ;; Suffix
       ((when-let* ((style (alist-get (aref pattern (1- len)) alist))
                    ((not (char-equal (aref pattern (max 0 (- len 2))) ?\\))))
          (cons style (substring pattern 0 -1))))))))

;; Recognizes the following patterns:
;; * .ext (file extension)
;; * regexp$ (regexp matching at end)
(defun +vertico-orderless-disambiguation-dispatch (word _index _total)
  (let ((tofu-re
         (if (boundp 'consult--tofu-regexp)
             (concat consult--tofu-regexp "*\\'")
           "\\'")))
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) tofu-re)))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) tofu-re))))))

(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-count 17)
  (vertico-resize nil)
  :config
  ;; Show a [CRM] indicator for `completing-read-multiple'.
  (defun +vertico-crm-indicator-a (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'+vertico-crm-indicator-a)

  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  ;; Doom binds these only with (evil +everywhere); Evil is everywhere here.
  (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)
  (define-key vertico-map (kbd "C-SPC") #'+vertico/embark-preview)
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-M-j") #'vertico-next-group)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "C-M-k") #'vertico-previous-group)
  ;; Doom's (cmds! (eq 'file ...) ...): only in file prompts.
  (define-key vertico-map (kbd "C-h")
    (lambda () (interactive)
      (when (eq 'file (vertico--metadata-get 'category))
        (call-interactively #'vertico-directory-up))))
  (define-key vertico-map (kbd "C-l")
    (lambda () (interactive)
      (when (eq 'file (vertico--metadata-get 'category))
        (call-interactively #'+vertico/enter-or-preview))))
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; These commands are problematic and automatically show the *Completions* buffer.
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defun +vertico--suppress-completion-help-a (fn &rest args)
    (cl-letf (((symbol-function #'minibuffer-completion-help) #'ignore))
      (apply fn args)))
  (advice-add #'ffap-menu-ask :around #'+vertico--suppress-completion-help-a))

(use-package orderless
  :config
  (setq orderless-affix-dispatch-alist
        '((?! . orderless-without-literal)
          (?& . orderless-annotation)
          (?% . char-fold-to-regexp)
          (?` . orderless-initialism)
          (?= . orderless-literal)
          (?^ . orderless-literal-prefix)
          (?~ . orderless-flex))
        orderless-style-dispatchers
        '(+vertico-orderless-dispatch
          +vertico-orderless-disambiguation-dispatch))

  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles orderless partial-completion)))
        orderless-component-separator #'orderless-escapable-split-on-space)
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

(use-package consult
  :init
  (define-key global-map [remap bookmark-jump] #'consult-bookmark)
  (define-key global-map [remap evil-show-marks] #'consult-mark)
  (define-key global-map [remap evil-show-registers] #'consult-register)
  (define-key global-map [remap goto-line] #'consult-goto-line)
  (define-key global-map [remap imenu] #'consult-imenu)
  (define-key global-map [remap Info-search] #'consult-info)
  (define-key global-map [remap locate] #'consult-locate)
  (define-key global-map [remap load-theme] #'consult-theme)
  (define-key global-map [remap recentf-open-files] #'consult-recent-file)
  (define-key global-map [remap switch-to-buffer] #'consult-buffer)
  (define-key global-map [remap switch-to-buffer-other-window] #'consult-buffer-other-window)
  (define-key global-map [remap switch-to-buffer-other-frame] #'consult-buffer-other-frame)
  (define-key global-map [remap yank-pop] #'consult-yank-pop)
  :config
  (setq consult-project-function
        (lambda (&optional _)
          (let ((proj (project-current nil)))
            (when proj (project-root proj))))
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult-source-recent-file consult-source-project-recent-file consult-source-bookmark
   :preview-key "C-SPC")
  (consult-customize
   consult-theme
   :preview-key '("C-SPC" :debounce 0.5 any))

  (defun +vertico--consult-recentf-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly.
`consult-buffer' needs `recentf-mode' to show file candidates."
    (recentf-mode +1))
  (advice-add #'consult-recent-file :before #'+vertico--consult-recentf-a)
  (advice-add #'consult-buffer :before #'+vertico--consult-recentf-a))

(use-package consult-dir
  :init
  (define-key global-map [remap list-directory] #'consult-dir)
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "C-x C-d") #'consult-dir)
    (define-key vertico-map (kbd "C-x C-j") #'consult-dir-jump-file))
  :config
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t)
  ;; Directory history picker (replaces the removed MELPA package
  ;; consult-dired-history; consult-dir already lists dired buffers,
  ;; recentf dirs and projects).
  (my-leader
   '(("d h" . consult-dir))))

(use-package consult-yasnippet
  :init
  (define-key global-map [remap yas-insert-snippet] #'consult-yasnippet))

(use-package embark
  :init
  (define-key global-map [remap describe-bindings] #'embark-bindings)
  (define-key global-map (kbd "C-;") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-;") #'embark-act)
  (define-key minibuffer-local-map (kbd "C-c C-;") #'embark-export)
  (define-key minibuffer-local-map (kbd "C-c C-l") #'embark-collect)
  (define-key minibuffer-local-map (kbd "C-c C-e") #'+vertico/embark-export-write)
  :config
  (require 'consult)
  ;; Doom's (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil).
  (add-to-list 'display-buffer-alist
               '("\\*Embark Export:"
                 (display-buffer-below-selected)
                 (window-height . 0.35)))
  (define-key embark-file-map (kbd "g") #'+vertico/embark-magit-status))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init
  (marginalia-mode 1)
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle)
  :config
  (dolist (cat '((flycheck-error-list-set-filter . builtin)
                 (persp-switch-to-buffer . buffer)
                 (projectile-switch-to-buffer . buffer)))
    (add-to-list 'marginalia-command-categories cat))
  ;; Doom's (+icons): nerd-icons in the marginalia annotations.
  (with-eval-after-load 'nerd-icons-completion
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (when (fboundp 'nerd-icons-completion-mode)
    (nerd-icons-completion-mode 1))
  (when (bound-and-true-p marginalia-mode)
    (nerd-icons-completion-marginalia-setup)))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))

;; Doom's (+childframe): M-x and friends pop up in a childframe.
;; NOTE: posframe cannot render in a terminal, so this stays off in emacs -nw.
;; NOTE 2: this must enable the mode explicitly: vertico-mode is already on
;; by the time this block runs, so a bare :hook would never fire.
(use-package vertico-posframe
  :config
  (defun my/vertico-posframe-maybe-enable ()
    "Enable `vertico-posframe-mode' on graphical displays only."
    (when (display-graphic-p)
      (vertico-posframe-mode 1)))
  (add-hook 'vertico-mode-hook #'my/vertico-posframe-maybe-enable)
  (my/vertico-posframe-maybe-enable))

;; From https://github.com/minad/vertico/wiki#candidate-display-transformations-custom-candidate-highlighting
;;
;; Uses `add-face-text-property' instead of `propertize' unlike the above
;; snippet because `append' is necessary to not override the match font lock.
;; See: minad/vertico#389
(use-package vertico-multiform
  :ensure nil
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory-fn (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +vertico-highlight-enabled-mode-fn (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and
                 (memq sym minor-mode-list)
                 (boundp sym)
                 (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory-fn)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode-fn))))

;; Doom's (:leader :desc "Actions" "a" #'embark-act).
(my-leader
 '(("a" . embark-act)))

;; Cape provides capf backends. There is no `cape-configure' any more, so
;; the backends are added to completion-at-point-functions directly. LSP
;; completion is wired in langs/lsp.el.
(use-package cape
  :init
  (add-hook 'completion-at-point-functions
            (lambda ()
              (add-completion-at-point-functions
               '(cape-abbrev cape-dabbrev cape-history cape-file cape-line)))
            nil t))

;;; ### Buffer management

;; Doom's (ibuffer +icons): consult takes the remaps (see above), so no
;; preview-position override like before.

;;; ### Files and directories

;; `project' is built into Emacs now, but it no longer exposes a mutable
;; `project-keymap'; bind the switch/delete/current commands directly.
(with-eval-after-load 'project
  ;; Make switching projects open its dired (Doom's behaviour with
  ;; projectile: the project's file tree appears).
  (setq project-switch-commands
        '((project-dired "Dired")
          (project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (project-vc-dir "VC-Dir")
          (project-eshell "Eshell"))))
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p k") #'project-delete-project)
(global-set-key (kbd "C-c p d") #'project-current)
(global-set-key (kbd "C-c p a") #'project-remember-project)
(global-set-key (kbd "C-c p l") #'my/project-list)

;; `project' remembers roots but only offers switching from a key binding: the
;; add/forget commands never made it into the menus and there is no list
;; command at all. Doom's project module had both, so provide them here.
;; `project-remember-project' interactively asks for a directory (via
;; `project-current' with a prompt) and accepts plain directories that are
;; not under version control as "transient" projects.
(require 'project)

(defvar my/project-list-buffer "*Projects*"
  "Buffer used by `my/project-list'.")

(define-derived-mode my/projects-mode tabulated-list-mode "Projects"
  "Major mode listing the projects `project' remembers.
\\<my/projects-mode-map>RET switches to the project on this line,
d forgets it, g refreshes the list."
  (setq tabulated-list-format [("Project" 0 t)]
        tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'my/projects--refresh nil t)
  (tabulated-list-init-header))

(defun my/projects--refresh ()
  "Fill the table with the known project roots."
  (setq tabulated-list-entries
        (mapcar (lambda (root) (list root (vector root)))
                (sort (copy-sequence (project-known-project-roots)) #'string<))))

(defun my/project-list ()
  "List the projects `project' remembers."
  (interactive)
  (let ((buffer (get-buffer-create my/project-list-buffer)))
    (with-current-buffer buffer
      (my/projects-mode)
      (tabulated-list-revert))
    (pop-to-buffer buffer)))

(defun my/projects-open ()
  "Switch to the project on the current line."
  (interactive)
  (let ((root (tabulated-list-get-id)))
    (unless root (user-error "No project on this line"))
    (project-switch-project root)))

(defun my/projects-forget ()
  "Forget the project on the current line."
  (interactive)
  (let ((root (tabulated-list-get-id)))
    (unless root (user-error "No project on this line"))
    (project-forget-project root)
    (tabulated-list-revert)))

(define-key my/projects-mode-map (kbd "RET") #'my/projects-open)
(define-key my/projects-mode-map (kbd "d") #'my/projects-forget)
(define-key my/projects-mode-map (kbd "a") #'project-remember-project)

;; Evil's normal-state map takes precedence over a local map, so `d' would run
;; evil-delete on a read-only buffer. Bind the actions for that state too, as
;; the dashboard does in ui.el.
(with-eval-after-load 'evil
  (evil-define-key 'normal my/projects-mode-map
    (kbd "RET") #'my/projects-open
    (kbd "d") #'my/projects-forget
    (kbd "a") #'project-remember-project
    ;; `g' is an evil prefix, so the mode's own `tabulated-list-revert'
    ;; binding would never run.
    (kbd "g") #'tabulated-list-revert))

;; ripgrep is consult's content search backend when installed; current
;; consult needs no setup call (the old consult-ripgrep-setup is gone).

;; NOTE: there is no `consult-fd' package on any current archive and
;; consult has no built-in fd commands. dirvish (below) covers finding
;; files interactively.

;;; ### Dired

;; dired is built into Emacs. Doom's (dired +icons +dirvish).
(eval-after-load 'dired
  (lambda ()
    (setq dired-listing-switches "-la")
    (add-hook 'dired-mode-hook #'dired-hide-details-mode)
    ;; Ported from config.el.
    (define-key dired-mode-map (kbd "N") #'dired-create-empty-file)
    (define-key dired-mode-map (kbd "R") #'dired-async-do-rename)
    (define-key dired-mode-map (kbd "C") #'dired-async-do-copy)
    (define-key dired-mode-map (kbd "S") #'dired-async-do-symlink)
    (define-key dired-mode-map (kbd "H") #'dired-async-do-hardlink)
    ;; dired-dwim-target, ported from config.el.
    (setq dired-dwim-target #'dired-dwim-target-recent)))

;; File manager inside Emacs, replacing dired's icons + dirvish module.
(use-package dirvish
  :bind (("C-c C-f" . dirvish-open)
         ("C-c C-d" . dirvish-dwim)
         ("C-c C-p" . dirvish-open-parent))
  :config
  (setq dirvish-default-layout '(0 0 0.4)
        dirvish-layout-recipes '((1 0.11 0.55) (0 0 0.40))
        dirvish-attributes '(vc-state subtree-state nerd-icons file-size)))

(use-package dired-open
  :commands (dired-open dired-open-xdg dired-open-browser dired-open-emacs))

(use-package dired-subtree
  :after dired
  :bind-keymap ("s" . dired-subtree-map))

;;; ### Large files

;; Doom's +large-files: vlf plus so-long, with vlf preserved across so-long.
(use-package vlf
  :commands (vlf)
  :custom
  (vlf-batch-size-remote read-process-output-max)
  :config
  (require 'vlf-setup)
  (add-hook 'vlf-mode-hook #'so-long-mode))

(use-package so-long
  :config
  (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)
  (global-so-long-mode 1))

;;; ### Customisation and bookmarks

;; custom.el holds generated Customize state, so it lives in XDG cache
;; instead of the repository.
(setq custom-file (expand-file-name "custom.el" my/emacs-cache))
(when (file-exists-p custom-file)
  (load custom-file nil t))

  ;; Dirvish (Doom's dired +icons +dirvish).
(use-package dirvish
  :commands (dirvish dirvish-side)
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-default-layout (quote (0 0 0.4))
        dirvish-layout-recipes
        (quote ((1 0.11 0.55) (2 0.22 0.55) (3 0.15 0.55)))
        dirvish-attributes
        (quote (vc-state subtree-state nerd-icons file-size))))


;;; ### Persistent scratch

;; The *scratch* buffer survives restarts: its contents are written to
;; XDG data on shutdown and reinserted at startup.
(defun my/scratch-state-file ()
  "Path where *scratch* contents are persisted across sessions."
  (expand-file-name "scratch-content.txt" my/emacs-data))

(defun my/save-scratch ()
  (when-let* ((buf (get-buffer "*scratch*"))
              ((> (buffer-size buf) 0)))
    (with-current-buffer buf
      (write-region (point-min) (point-max)
                    (my/scratch-state-file) nil 'silent))))

(defun my/restore-scratch ()
  (let ((file (my/scratch-state-file))
        (buf (get-buffer "*scratch*")))
    (when (and buf (file-exists-p file)
               (not (buffer-modified-p buf)))
      (with-current-buffer buf
        (goto-char (point-min))
        (insert-file-contents file)))))

(add-hook 'kill-emacs-hook #'my/save-scratch)
(add-hook 'emacs-startup-hook #'my/restore-scratch)

(savehist-mode +1)

(provide 'my-buffers)
;;; my-buffers.el ends here

