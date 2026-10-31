;;; my-keys.el -*- lexical-binding: t; -*-
;;; my-keys.el --- Modal editing (Meow) and key bindings

;;; Commentary:
;; Ported from Doom's config.el.
;;
;; Meow replaces the Evil stack. It is a selection-first modal layer on
;; top of vanilla Emacs keymaps: normal state for editing buffers,
;; motion state for read-only/special buffers, insert state for
;; terminals. See `meow-mode-state-list' below.
;;
;; `SPC' keeps opening the Doom-style leader defined in this file. Meow
;; binds `SPC' to its keypad by default, so the leader is rebound to
;; `my-leader-map' and the keypad moves to `M-SPC'.
;;
;; The old `general' package was dropped on purpose: its leader-key API
;; is gone in the current release, and plain keymaps are easier to audit
;; during a migration.

;;; Code:

;;; ### use-package defaults

(setq use-package-always-ensure t
      use-package-always-update-on-changelog nil
      use-package-always-pin nil)

;;; ### Meow

;; Meow replaces Evil. It is selection-first and sits on top of vanilla
;; keymaps, so most package bindings keep working unchanged. The leader
;; and the state bindings are configured in `my/meow-setup'.
(use-package meow
  :ensure t
  :config
  (defun my/meow-setup ()
    "Doom-like QWERTY layout for Meow."
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    (meow-define-keys 'motion
      '("j" . meow-next)
      '("k" . meow-prev))

    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  (my/meow-setup)

  ;; States for buffers where a package owns the keys. Terminals go
  ;; straight to insert; read-only modes get Meow's sparse motion map so
  ;; their own bindings (dired, magit, elfeed, ...) keep working.
  (dolist (entry
           '((vterm-mode . insert)
             (term-mode . insert)
             (eshell-mode . insert)
             (comint-mode . insert)
             (special-mode . motion)
             (magit-mode . motion)
             (dired-mode . motion)
             (wdired-mode . normal)
             (elfeed-search-mode . motion)
             (elfeed-show-mode . motion)
             (pdf-view-mode . motion)
             (nov-mode . motion)
             (org-agenda-mode . motion)
             (treemacs-mode . motion)
             (dashboard-mode . motion)
             (calibredb-search-mode . motion)
             (detached-list-mode . motion)
             (my/projects-mode . motion)))
    (add-to-list 'meow-mode-state-list entry))

  (meow-global-mode 1))

;; Text scale is not modal; keep the old global shortcuts.
(global-set-key (kbd "C-=") #'text-scale-increase)
(global-set-key (kbd "C--") #'text-scale-decrease)

;;; ### Leader

;; Replaces Doom's `map! :leader'. Every prefix the user already knows
;; is defined here so the rest of the configuration can bind into it.
(defvar my-leader-map (make-sparse-keymap)
  "Keymap for the leader key (SPC).")

;; SPC opens the leader only in Meow normal/motion state, never in insert
;; state or the minibuffer, so SPC keeps inserting a space while typing.
;; The actual bindings live after `my-leader' because the map must exist
;; first.

;; Tab navigation moved off Evil's `g t'/`g T' (Meow uses `g' for
;; cancel-selection) to `C-c t'/`C-c T'.

(defun my-leader (bindings)
  "Bind BINDINGS, a quoted alist of (KEY . COMMAND), in `my-leader-map'.

KEY is a string understood by `kbd'; spaces are key separators, so
\"o p\" binds the two-key sequence `o', `p'. The token \"SPC\" denotes
the leader key itself."
  (declare (indent defun))
  (dolist (binding bindings)
    ;; `kbd' already understands the spaces Doom uses as separators, so
    ;; "SPC ." becomes the two-key sequence <SPC> then ".".
    (define-key my-leader-map (kbd (car binding)) (cdr binding))))

;;; ### Meow bindings that need `my-leader-map'

;; Meow is loaded above, but the leader map is defined here. Bind SPC to
;; the existing Doom-style leader, move Meow's keypad to M-SPC, and keep
;; the old workspace and redo shortcuts on the normal-state map.
(with-eval-after-load 'meow
  (define-key meow-normal-state-keymap (kbd "SPC") my-leader-map)
  (define-key meow-motion-state-keymap (kbd "SPC") my-leader-map)
  (define-key meow-normal-state-keymap (kbd "M-SPC") #'meow-keypad)
  (define-key meow-motion-state-keymap (kbd "M-SPC") #'meow-keypad)
  (define-key meow-normal-state-keymap (kbd "C-t") #'new)
  (define-key meow-normal-state-keymap (kbd "C-S-t") #'display)
  (define-key meow-normal-state-keymap (kbd "C-r") #'undo-fu-only-redo)
  ;; Completion is not modal: the same key works in both states. Corfu
  ;; shows the candidates, and Cape provides the extra backends.
  (define-key meow-normal-state-keymap (kbd "C-M-i") #'completion-at-point)
  (define-key meow-motion-state-keymap (kbd "C-M-i") #'completion-at-point))

;; NOTE: BINDINGS must be quoted. A literal dotted pair in argument
;; position is evaluated as a function call, so (\"o -\" . #'dired-jump)
;; would try to call the string \"o -\".
(my-leader
 '(("SPC" . consult-buffer)
   ("?"   . meow-cheatsheet)
   ("o -" . dired-jump)
   ("o p" . project-dired)
   ("o f" . find-file)
   ("o r" . recentf)
   ;; Doom keeps project switching on SPC p p; SPC o t is the terminal.
   ("p p" . project-switch-project)
   ;; Adding and listing projects: `project-remember-project' prompts for a
   ;; directory, `my/project-list' browses the remembered ones (buffers.el).
   ("p a" . project-remember-project)
   ("p l" . my/project-list)
   ("p d" . project-forget-project)
   ;; Evil exposed marks/registers through `' and `"; Meow has no
   ;; equivalent state commands, so keep consult's pickers under search.
   ("s m" . consult-mark)
   ("s r" . consult-register)))

;;; ### Cape (completion at point)

;; Cape backends are registered in buffers.el. These keys run them
;; interactively from normal state, where typing a word first is not an
;; option. `C-M-i' keeps the plain `completion-at-point' entry point.
(my-leader
 '(("i d" . cape-dabbrev)
   ("i f" . cape-file)
   ("i l" . cape-line)
   ("i h" . cape-history)
   ("i s" . cape-elisp-symbol)
   ("i b" . cape-elisp-block)
   ("i a" . cape-abbrev)))

;;; ### Documentation

;; The configuration manual is README.org, one level above the config
;; directory in the dotfiles repository. Resolve the symlink first so
;; this also works when ~/.config/emacs is stowed.
(defun my/open-config-docs ()
  "Open *manual.org* – manual de referencia de la configuración.
  Este archivo se localiza en `../../docs/manual.org` con respecto
  a la raíz del repositorio de dotfiles." 
  (interactive)
  (let ((file (expand-file-name "../../docs/manual.org"
                                (file-truename my/config-dir))))
    (if (file-exists-p file)
        (find-file file)
      (user-error "Manual no encontrado: %s" file))))

;; `SPC h' mirrors the usual help prefix: the manual, Info manuals and
;; the describe commands, all reachable from Meow normal state.
(my-leader
 '(("h r" . my/open-config-docs)
   ("h i" . info)
   ("h m" . info-display-manual)
   ("h f" . describe-function)
   ("h v" . describe-variable)
   ("h k" . describe-key)
   ("h p" . describe-package)
   ("h o" . describe-symbol)
   ("h b" . describe-bindings)))

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
  ;; they apply no matter which Meow state map looked SPC up. Keep in
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
    "SPC h"   "help"
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
    ;; help and documentation
    "SPC h r" "config manual"
    "SPC h i" "Info manuals"
    "SPC h m" "Info manual by name"
    "SPC h f" "describe function"
    "SPC h v" "describe variable"
    "SPC h k" "describe key"
    "SPC h p" "describe package"
    "SPC h o" "describe symbol"
    "SPC h b" "bindings"
    ;; open
    "SPC o -" "dired here"
    "SPC o e" "feeds"
    "SPC o f" "file"
    "SPC o p" "project tree"
    "SPC o r" "recent files"
    "SPC o u" "URL as org"
    "SPC o t" "terminal"
    "SPC o P" "passwords"
    "SPC o s" "sessions"
    "SPC o S" "list sessions"
    "SPC o C" "credentials"
    "SPC o A" "add credential"
    "SPC o W f" "focus section"
    "SPC o W s" "split view"
    "SPC o W w" "toggle"
    "SPC o W q" "exit focus"
    ;; search and git
    "SPC ?"   "cheatsheet"
    "SPC s s" "ripgrep"
    "SPC s l" "line"
    "SPC s b" "line, current buffer"
    "SPC s m" "marks"
    "SPC s r" "registers"
    "SPC g g" "status"
    "SPC g d" "dispatch"
    ;; tree, check, capture, act, completion
    "SPC t t" "toggle"
    "SPC t k" "keycast"
    "SPC c l" "correct text"
    "SPC c c" "compile"
    "SPC c C" "recompile"
    "SPC c s" "spelling"
    "SPC c j" "jinx"
    "SPC j c" "capture"
    "SPC a" "act on thing"
    "SPC i" "complete at point"
    "SPC i d" "dabbrev"
    "SPC i f" "file name"
    "SPC i l" "line from buffer"
    "SPC i h" "history"
    "SPC i s" "elisp symbol"
    "SPC i b" "elisp block"
    "SPC i a" "abbrev"
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
    "SPC m d" "image from clipboard"
    "SPC m D" "image from URL"
    "SPC m E" "pandoc export"
    ;; notes
    "SPC n d" "search notes"
    "SPC n p" "pomodoro"
    "SPC n P" "stop pomodoro"
    "SPC n l" "insert link"
    "SPC n w" "web page"
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
    "SPC l S" "symbols"
    "SPC l F" "file symbols"
    "SPC l E" "diagnostics"
    "SPC l e" "file diagnostics"
    "SPC l t i" "install all"
    "SPC l t l" "list"
    "SPC l t d" "doctor"
    "SPC l t e" "ensure installed"
    ;; writing
    "SPC x w" "writer room"
    "SPC x o" "centered text"
    "SPC x m" "mixed pitch"
    "SPC x j" "jinx toggle"
    "SPC x h" "hugo"
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
    "C-c t"   "tab right"
    "C-c T"   "tab left"
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
   ("b k"  . kill-current-buffer)
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
   ;; `k' is windmove-up, so kill-buffer-and-window takes the shifted key.
   ("w K" . kill-buffer-and-window)
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
  "SPC w K" "kill buffer + window"
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
      (insert (format "meow state: %s\n" meow--current-state))
      (insert (format "current-local-map SPC: %S\n" (lookup-key (current-local-map) (kbd "SPC"))))
      (insert (format "global SPC: %S\n" (lookup-key global-map (kbd "SPC"))))
      (insert (format "meow-normal SPC: %S\n" (lookup-key meow-normal-state-keymap (kbd "SPC"))))
      (insert (format "meow-motion SPC: %S\n" (lookup-key meow-motion-state-keymap (kbd "SPC"))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))
