;;; my-keys.el -*- lexical-binding: t; -*-
;;; my-keys.el --- Modal editing and key bindings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's config.el.
;;
;; NOTE: the current Evil (1.15) renamed its variables and keymaps:
;; `evil-normal-map' is now `evil-normal-state-map', `evil-escape-delay'
;; became `evil-esc-delay', and `evil-escape-key-sequence' no longer
;; exists. The third-party `evil-escape' package restores that variable,
;; so the "kj" escape sequence the user is used to is preserved.
;;
;; The `general' package was dropped on purpose: its leader-key API is
;; gone in the current release, and plain keymaps are easier to audit
;; during a migration.

;;; Code:

;;; ### use-package defaults

(setq use-package-always-ensure t
      use-package-always-update-on-changelog nil
      use-package-always-pin nil)

;;; ### Evil

;; Doom's (evil +everywhere): the modal map replaces emacs-mode-map in
;; every buffer except the ones listed below.
(setq-default evil-move-cursor-back nil
      evil-want-fine-undo t
      evil-case-fold-search t)

;; NOTE: package variables belong in :custom, not :init. :init runs
;; before the package is loaded, so a defcustom would overwrite the value.
(use-package evil-escape
  :custom
  (evil-escape-key-sequence "kj")
  (evil-escape-delay 0.1)
  :config
  (evil-escape-mode 1))

;; NOTE: Evil 1.15 removed evil-mode-disable. Disabling Evil in a buffer
;; is now done by listing the major mode in `evil-emacs-state-modes',
;; which already covers magit-*, org-agenda-mode, ediff-mode, pdf-view-mode,
;; occur-mode, package-menu-mode, recentf-dialog-mode and the reftex modes.
;; `evil-buffer-regexps' is the equivalent for buffers matched by name.
(use-package evil
  :custom
  ;; Keep only Evil's built-in emacs state entries so the modes the user
  ;; works with daily (dired, text, org, magit, ...) become modal rather
  ;; than staying in "holy"/Emacs state.
  (evil-emacs-state-modes nil)
  (evil-default-state 'normal)

  :config
  (define-key evil-normal-state-map (kbd "<M-left>") #'evil-backward-word)
  (define-key evil-normal-state-map (kbd "<M-right>") #'evil-forward-word)
  (global-set-key (kbd "C-=") 'text-scale-increase)
  (global-set-key (kbd "C--") 'text-scale-decrease)
  ;; Turn Evil on. Evil has no global minor mode, so it is enabled here
  ;; and re-enabled for every new frame; evil-escape-mode does nothing
  ;; useful while evil-mode is off.
  ;; NOTE: there is no evil-set-default-keybindings in Evil 1.15; the
  ;; default bindings are installed by evil-mode itself.
  (evil-mode 1)
  (add-hook 'after-make-frame-functions #'evil-mode)

  ;; evil-collection restores Doom's "evil everywhere" behaviour: it adds
  ;; the right bindings/motion to magit, dired, org, term, help, etc.
  ;; and moves them out of pure Emacs state.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

  ;; Evil 1.15 declares evil-mode-buffers with (defvar evil-mode-buffers),
  ;; which marks it special but leaves it void, and evil-normal-post-command
  ;; reads it through evil-initializing-p on every command in Normal state:
  ;;   Error in post-command-hook (evil-normal-post-command):
  ;;   (void-variable evil-mode-buffers)
  ;; Verified present in both NonGNU ELPA and MELPA. Give it a value
  ;; until upstream fixes the declaration.
  (unless (boundp 'evil-mode-buffers)
    (setq evil-mode-buffers nil)))

;;; ### Leader

;; Replaces Doom's `map! :leader'. Every prefix the user already knows
;; is defined here so the rest of the configuration can bind into it.
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for the leader key (SPC).")

;; Doom-like modal leader: SPC opens the leader ONLY in Evil's
;; normal/visual/motion/emacs states. It is deliberately NOT bound in
;; `global-map', in local maps, or in insert state, so SPC keeps inserting
;; a space while typing (minibuffer narrowing like "find file" needs it).
(defconst my-leader-state-maps
  '(evil-normal-state-map evil-visual-state-map
    evil-motion-state-map evil-emacs-state-map)
  "Evil state maps where SPC opens `my-leader-map'.")

(with-eval-after-load 'evil
  (dolist (state-map my-leader-state-maps)
    (define-key (symbol-value state-map) (kbd "SPC") my-leader-map)))

;; Restore Vim's default tab navigation (works on the visible tab bar),
;; rather than a custom command.
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "g t") #'centaur-tabs-forward-tab)
  (define-key evil-normal-state-map (kbd "g T") #'centaur-tabs-backward-tab))

(defun my-leader (bindings)
  "Bind BINDINGS, a quoted alist of (KEY . COMMAND), in `my-leader-map'.

KEY is a string that may contain spaces (used as separators, Doom
style); spaces are removed before binding, so \"o p\" binds the two
key sequence `o', `p'. The token \"SPC\" denotes the leader key itself."
  (declare (indent defun))
  (dolist (binding bindings)
    ;; `kbd' already understands the spaces Doom uses as separators, so
    ;; "SPC ." becomes the two-key sequence <SPC> then ".".
    (define-key my-leader-map (kbd (car binding)) (cdr binding))))

;; NOTE: BINDINGS must be quoted. A literal dotted pair in argument
;; position is evaluated as a function call, so (\"o -\" . #'dired-jump)
;; would try to call the string \"o -\".
(my-leader
 '(("SPC" . consult-buffer)
   ("o -"  . dired-jump)
   ("o p"  . project-dired)
   ("o f"  . find-file)
   ("o r"  . recentf)
   ;; Doom keeps project switching on SPC p p; SPC o t is the terminal.
   ("p p"  . project-switch-project)
   ;; Adding and listing projects: `project-remember-project' prompts for a
   ;; directory, `my/project-list' browses the remembered ones (buffers.el).
   ("p a"  . project-remember-project)
   ("p l"  . my/project-list)
   ("p d"  . project-forget-project)))

;;; ### which-key

;; Built into Emacs 31; configured to appear as quickly as in Doom.
(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0)
  (which-key-max-display-length 30)
  (which-key-setup-side-window-right)
  :config
  (which-key-mode 1)
  ;; Name the leader prefixes. The leader groups are plain
  ;; `make-sparse-keymap' entries created by `my-leader', which carry no
  ;; description, so which-key labels every one of them "+prefix" in the
  ;; popup. Key-based replacements key off the *displayed* sequence, so
  ;; they apply no matter which evil state map looked SPC up. Keep in
  ;; sync with the prefixes bound in this file and the modules
  ;; (buffers.el, editing.el, lsp.el, treesit.el, org.el, languagetool.el).
  ;; NOTE: no leading "+" in these labels. which-key already prepends
  ;; `which-key-prefix-prefix' ("+") to every keymap entry, so "+buffers"
  ;; here rendered as "++buffers" in the popup.
  (which-key-add-key-based-replacements
    "SPC b"   "buffers"
    "SPC c"   "check"
    "SPC d"   "dirs"
    "SPC f"   "files"
    "SPC g"   "git"
    "SPC l"   "lsp"
    "SPC l t" "treesit"
    "SPC m"   "doc"
    "SPC n"   "notes"
    "SPC n j" "journal"
    "SPC n r" "roam"
    "SPC o"   "open"
    "SPC o W" "writer"
    "SPC p"   "project"
    "SPC q"   "quit"
    "SPC s"   "search"
    "SPC TAB" "workspace"
    "SPC t"   "tree"
    "SPC w"   "windows"
    "SPC x"   "writing"
    "SPC y"   "ai")

  ;; Leaf labels. With no replacement which-key prints the command name, so
  ;; the popup repeats the page ("persp-switch-to-buffer" under +buffers,
  ;; "org-roam-node-find" under +roam, "my/next-buffer" anywhere) instead of
  ;; saying what the key does. The page header already names the topic; each
  ;; line is only the action. Deferred bindings (detached, writer) are listed
  ;; too: replacements do not depend on the command being loaded.
  (which-key-add-key-based-replacements
    ;; buffers
    "SPC b b" "switch in workspace"
    "SPC b B" "switch in all"
    "SPC b I" "list in workspace"
    "SPC b l" "list all"
    "SPC b k" "kill"
    "SPC b K" "kill others"
    "SPC b n" "next"
    "SPC b p" "previous"
    ;; files
    "SPC f f" "open"
    "SPC f r" "recent"
    "SPC f p" "find in project"
    ;; project
    "SPC p p" "switch"
    "SPC p a" "add"
    "SPC p l" "list"
    "SPC p d" "forget"
    ;; open
    "SPC o -" "dired here"
    "SPC o e" "feeds"
    "SPC o f" "file"
    "SPC o p" "project tree"
    "SPC o r" "recent files"
    "SPC o t" "terminal"
    "SPC o P" "passwords"
    "SPC o s" "sessions"
    "SPC o S" "list sessions"
    "SPC o W f" "focus section"
    "SPC o W s" "split view"
    "SPC o W w" "toggle"
    "SPC o W q" "exit focus"
    ;; search and git
    "SPC s s" "ripgrep"
    "SPC s l" "line"
    "SPC s b" "line, current buffer"
    "SPC g g" "status"
    "SPC g d" "dispatch"
    ;; tree, check, capture, act
    "SPC t t" "toggle"
    "SPC t k" "keycast"
    "SPC c l" "correct text"
    "SPC c c" "compile"
    "SPC c C" "recompile"
    "SPC j c" "capture"
    "SPC a" "act on thing"
    "SPC SPC" "buffer"
    "SPC <" "buffer in all"
    "SPC ," "buffer in workspace"
    ;; doc
    "SPC m a" "attach"
    "SPC m i" "clock in"
    "SPC m o" "clock out"
    "SPC m c" "clock goto"
    "SPC m x" "clock cancel"
    "SPC m e" "export"
    "SPC m v" "view"
    "SPC m p" "live preview"
    "SPC m s" "slides"
    ;; notes
    "SPC n d" "search notes"
    "SPC n p" "pomodoro"
    "SPC n P" "stop pomodoro"
    "SPC n r f" "find node"
    "SPC n r i" "insert link"
    "SPC n r c" "capture"
    "SPC n r l" "backlinks"
    "SPC n j p" "today"
    "SPC n j w" "work"
    ;; lsp and treesit
    "SPC l i" "install servers"
    "SPC l l" "list servers"
    "SPC l s" "status"
    "SPC l d" "doctor"
    "SPC l +" "install for this mode"
    "SPC l c" "dictionary"
    "SPC l t i" "install all"
    "SPC l t l" "list"
    "SPC l t d" "doctor"
    "SPC l t e" "ensure installed"
    ;; writing
    "SPC x w" "writer room"
    "SPC x o" "centered text"
    "SPC _ t" "synonyms"
    ;; ai
    "SPC y i" "install models"
    "SPC y c" "chat"
    "SPC y w" "write"
    "SPC y t" "translate"
    "SPC y s" "summarize"
    "SPC y r" "review code"
    "SPC y C" "complete code"
    "SPC y a" "add code"
    "SPC y e" "edit code"
    "SPC y p" "proofread"
    "SPC y g" "fix grammar"
    "SPC y d" "define word"
    "SPC y l" "make list"
    "SPC y P" "provider"
    ;; dirs and docker
    "SPC d d" "containers"
    "SPC d h" "directory"
    ;; quit
    "SPC q q" "quit"
    "SPC q Q" "quit, no save"
    "SPC q s" "save session"
    "SPC q l" "load session"
    ;; workspaces
    "SPC TAB TAB" "list"
    "SPC TAB ." "switch to"
    "SPC TAB `" "last used"
    "SPC TAB n" "new"
    "SPC TAB N" "new named"
    "SPC TAB l" "load saved"
    "SPC TAB s" "save"
    "SPC TAB x" "clear session"
    "SPC TAB d" "delete"
    "SPC TAB D" "delete saved"
    "SPC TAB r" "rename"
    "SPC TAB R" "restore session"
    "SPC TAB ]" "right"
    "SPC TAB [" "left"
    "SPC TAB 1" "workspace 1"
    "SPC TAB 2" "workspace 2"
    "SPC TAB 3" "workspace 3"
    "SPC TAB 4" "workspace 4"
    "SPC TAB 5" "workspace 5"
    "SPC TAB 6" "workspace 6"
    "SPC TAB 7" "workspace 7"
    "SPC TAB 8" "workspace 8"
    "SPC TAB 9" "workspace 9"
    "SPC TAB 0" "last workspace")

  ;; Prefixes outside the leader that which-key also renders. Same rule: the
  ;; prefix says where the keys live, each entry says what it does.
  (which-key-add-key-based-replacements
    "C-c p"   "project"
    "C-c p p" "switch"
    "C-c p a" "add"
    "C-c p l" "list"
    "C-c p k" "delete"
    "C-c p d" "current"
    "C-c m"   "multiple cursors"
    "C-c g"   "status"
    "C-c C-g" "dispatch"
    "C-c M-g" "file dispatch"
    "C-c C-f" "open directory"
    "C-c C-d" "directory at point"
    "C-c C-p" "parent directory"
    "C-c e"   "chat"
    "M-&"     "shell command"
    ;; `M-<digit>' is the workspace switcher (see ui.el); the command names
    ;; are `switch-to-0'..`switch-to-8', counted from zero.
    "M-1" "workspace 1"
    "M-2" "workspace 2"
    "M-3" "workspace 3"
    "M-4" "workspace 4"
    "M-5" "workspace 5"
    "M-6" "workspace 6"
    "M-7" "workspace 7"
    "M-8" "workspace 8"
    "M-9" "workspace 9"
    "M-0" "last workspace"))


(provide 'my-keys)
;;; my-keys.el ends here

;; ### Doom-like common bindings (basic)
(my-leader
 '(("SPC" . consult-buffer)
   ;; Doom's workspaces module replaces the plain buffer switchers on the
   ;; buffer map with workspace-aware ones.
   ("b b"  . persp-switch-to-buffer)
   ("b B"  . switch-to-buffer)
   ("b I"  . buffers)
   ("b k"  . kill-this-buffer)
   ("b K"  . (lambda () (interactive) (mapc #'kill-buffer (delq (current-buffer) (buffer-list)))))
   ("b l"  . ibuffer)
   ;; Workspace-aware cycling; defined in ui.el next to the workspace
   ;; library. Plain `next-buffer' is unusable here: persp-mode's frame
   ;; `buffer-predicate' caches the buffer list for two seconds and the
   ;; built-in walk stops on the first buffer the stale list allows.
   ("b n"  . my/next-buffer)
   ("b p"  . my/previous-buffer)
   ("f f"  . find-file)
   ("f r"  . recentf-open-files)
   ("f p"  . project-find-file)
   ("g g"  . magit-status)
   ("g d"  . magit-dispatch)
   ("s b"  . consult-line)
   ("s l"  . consult-line)
   ("s s"  . consult-ripgrep)
   ("t t"  . treemacs)
   ("w /"  . split-window-right)
   ("w -"  . split-window-below)
   ("w d"  . delete-window)
   ("w m"  . delete-other-windows)
   ("q q"  . save-buffers-kill-terminal)
   ("q Q"  . (lambda () (interactive) (kill-emacs)))
   ("o -"  . dired-jump)))

;; ### Doom window bindings
(my-leader
 '(("w h" . windmove-left)
   ("w j" . windmove-down)
   ("w k" . windmove-up)
   ("w l" . windmove-right)
   ("w s" . split-window-below)
   ("w v" . split-window-right)
   ("w c" . delete-window)
   ("w d" . delete-window)
   ("w m" . delete-other-windows)
   ("w u" . winner-undo)
   ("w U" . winner-redo)))

;; The `SPC w' page already says the keys manage windows, so which-key only
;; needs the action. Without replacements it repeats the command names
;; ("windmove-left", "split-window-right", "delete-other-windows", ...),
;; which restate the page and hide what the key actually does.
(which-key-add-key-based-replacements
  "SPC w h" "left"
  "SPC w j" "down"
  "SPC w k" "up"
  "SPC w l" "right"
  "SPC w s" "split below"
  "SPC w -" "split below"
  "SPC w v" "split right"
  "SPC w /" "split right"
  "SPC w d" "close"
  "SPC w c" "close"
  "SPC w m" "close others"
  "SPC w u" "undo"
  "SPC w U" "redo")


(defun my-leader-sanity-check ()
  "Open a diagnostic buffer for SPC bindings."
  (interactive)
  (let ((buffer (get-buffer-create "*leader-check*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "major-mode: %s\n" (symbol-name major-mode)))
      (insert (format "current-local-map SPC: %S\n" (lookup-key (current-local-map) (kbd "SPC"))))
      (insert (format "global SPC: %S\n" (lookup-key global-map (kbd "SPC"))))
      (insert (format "evil-normal-state-map SPC: %S\n" (lookup-key evil-normal-state-map (kbd "SPC"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))
