;;; my-ui.el -*- lexical-binding: t; -*-
;;; my-ui.el --- Theme, modeline, tabs, fonts & chrome

;;; Commentary:
;; Ported from Doom's config.el and :ui. Uses gruvbox-dark-hard; fonts,
;; modeline, tabs, line numbers, blink cursor kept as in Doom.

;;; Code:

(require 'cl-lib)

;;; ### Theme

;; gruvbox-material is not published on any current archive; the base
;; gruvbox-theme is, and it is the theme the user asked for. The theme
;; must be loaded before anything that customises faces.
(require 'gruvbox-theme)
(load-theme 'gruvbox-dark-hard t)
(setq gruvbox-style-diagnostics 'fancy)

;; Icons for buffer names, mode lines and the like.
(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.95
        all-the-icons-default-adjust -0.2))

;;; ### Fonts

;; Same fonts as the Doom config: Cascadia Mono NF for code, Alegreya for
;; prose (variable-pitch in Org).
(when (display-graphic-p)
  (setq doom-font (font-spec :family "CaskaydiaMono NF" :size 15)
        doom-variable-pitch-font (font-spec :family "Alegreya" :size 18)
        doom-big-font (font-spec :family "CaskaydiaMono NF" :size 22))

  ;; The historical Doom config named the family "Cascadia Mono NF", but
  ;; this host's installed font is spelled "CaskaydiaMono NF". Apply it to
  ;; the `default' face instead of only to the dead `doom-font' variable.
  (set-face-attribute 'default nil :font doom-font)
  (set-frame-font doom-font t t)

  (set-face-attribute 'fixed-pitch nil :font doom-font)
  (set-face-attribute 'mode-line nil
                      :height 130 :inherit 'variable-pitch)
  (set-face-attribute 'mode-line-inactive nil
                      :height 120 :inherit 'variable-pitch))

;;; ### Modeline

;; doom-modeline is a standalone package with the same entry point as Doom's,
;; so the mode toggles used elsewhere keep working.
(use-package doom-modeline
  :custom
  (doom-modeline-major-mode-icon t)
  (doom-modeline-lsp-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-percent-position t)
  (doom-modeline-lsp t)
  (doom-modeline-workspace-name t)
  :config
  (doom-modeline-mode +1))

;;; ### Tabs

;; Doom's (tabs +centaur +workspace).
(use-package centaur-tabs
  :custom
  (centaur-tabs-style "alternate")
  (centaur-tabs-height 28)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-set-icons t)
  :config
  ;; Doom only shows the tab bar once a real file is opened, not at
  ;; startup (the dashboard would otherwise show an empty tab bar).
  (defun my/centaur-tabs-maybe-enable ()
    "Enable centaur-tabs once a file-visiting buffer exists."
    (unless centaur-tabs-mode
      (when (cl-some #'buffer-file-name (buffer-list))
        (centaur-tabs-mode +1))))
  (add-hook 'find-file-hook #'my/centaur-tabs-maybe-enable)
  (add-hook 'after-save-hook #'my/centaur-tabs-maybe-enable)
  ;; The dashboard must never show the tab bar. Centaur-tabs checks
  ;; this predicate every time it renders the bar, so the dashboard
  ;; stays clean no matter when centaur-tabs-mode is enabled.
  (setq centaur-tabs-hide-predicate
        (lambda ()
          (string-match-p "\\*dashboard\\*" (buffer-name)))))

;;; ### Workspaces

;; Doom's (workspaces +layouts): persisted, per-buffer window
;; configurations, exposed with Doom's `SPC TAB' key bindings on top of
;; `persp-mode'. Commands keep short action names (`new', `switch-to',
;; `kill', ...) because the key prefix already identifies the workspace
;; context.
;;
;; NOTE: `persp-keymap', `persp-show-branches',
;; `persp-keymap-save-environment' and `persp-initial-frame-workspace' are
;; not persp-mode variables. The real ones are `persp-keymap-prefix' and
;; `persp-names-cache'; Doom's actual settings are applied below.
;;
;; persp-mode defines the `gv' setters for its struct accessors. Load it at
;; compile time so `(setf (persp-hidden ...))' expands correctly even when
;; this file is byte-compiled outside a full init.
(eval-when-compile (require 'persp-mode nil t))

(defvar main-workspace "main"
  "Name of the primary workspace, created at startup if missing.")

(defvar workspaces-data-file "_workspaces"
  "Basename of the file where `save' stores workspaces.
Like Doom's `+workspaces-data-file', it lives in `persp-save-dir'.")

(defvar last-workspace main-workspace
  "Previously active workspace, used by `other'.")

(use-package persp-mode
  :custom
  ;; The default is (persp-confs/ user-emacs-directory), which is a symlink
  ;; into the dotfiles git repository. Keep workspace state out of it.
  (persp-save-dir (file-name-as-directory
                   (expand-file-name "persp-confs" my/emacs-data)))
  :config
  ;; persp-mode does NOT create this directory; every save or workspace
  ;; switch fails with "No such file or directory" until it exists.
  (unless (file-directory-p persp-save-dir)
    (make-directory persp-save-dir t))

  ;; Settings from Doom's workspaces module. `persp-auto-resume-time' stays
  ;; at 0 (never auto-restore) so the last project does not take over
  ;; startup; `SPC q l' and `SPC TAB R' restore the session on demand.
  ;; `persp-nil-hidden' is obsolete and ignored in persp-mode 3.9.1+, so the
  ;; nil workspace is hidden through its struct slot in
  ;; `ensure-main-workspace' instead.
  (setq persp-autokill-buffer-on-remove 'kill-weak
        persp-reset-windows-on-nil-window-conf nil
        persp-auto-save-fname "autosave"
        persp-auto-save-opt (if noninteractive 0 1)
        persp-auto-resume-time 0
        persp-set-last-persp-for-new-frames t
        persp-switch-to-added-buffer nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-remove-buffers-from-nil-persp-behaviour nil)

  ;; Doom's prefix was `SPC TAB'; persp-mode's own default, `C-c p', collides
  ;; with the `project' bindings in buffers.el (`C-c p p', `C-c p k',
  ;; `C-c p d').
  (customize-set-variable 'persp-keymap-prefix (kbd "C-c C-w"))

  ;;; Workspace library (equivalents of Doom's `+workspace/*' commands)

  (defun list-workspace-names ()
    "Return the names of the open workspaces, in display order."
    (cl-remove persp-nil-name (persp-names-current-frame-fast-ordered)
               :count 1))

  (defun workspace-exists-p (name)
    "Return non-nil when NAME is an open workspace."
    (member name (list-workspace-names)))

  (defun current-workspace-name ()
    "Return the name of the current workspace."
    (persp-name (persp-get-current)))

  (defun find-workspace (name &optional noerror)
    "Return the workspace named NAME.
Signal an error unless NOERROR is non-nil."
    (let ((persp (persp-get-by-name name)))
      (cond ((perspective-p persp) persp)
            ((not noerror)
             (error "No workspace called '%s' was found" name)))))

  (defun workspace-buffers (&optional workspace)
    "Return the buffers belonging to WORKSPACE (default: the current one)."
    (let ((persp (or workspace (persp-get-current))))
      (if (perspective-p persp)
          (persp-buffers persp)
        (buffer-list))))

  ;;; Buffer navigation

  (defun my/switch-buffer (n)
    "Switch N buffers forward in the current workspace.
A negative N switches backward instead.

`next-buffer' and `previous-buffer' honour the frame's `buffer-predicate',
which is how the walk stays inside the workspace. persp-mode installs a
predicate whose buffer list is cached for two seconds, so the built-in
commands reject buffers opened in the meantime and then stop dead on the
first buffer the stale list allows. Run the walk against a predicate
derived from the workspace list as it is right now."
    (let ((members (seq-filter #'buffer-live-p (workspace-buffers))))
      (cond
       ((null members)
        (user-error "No buffers in this workspace"))
       ((null (cdr members))
        (user-error "No other buffers in this workspace"))
       (t
        (let ((pred (frame-parameter nil 'buffer-predicate)))
          (unwind-protect
              (progn
                (set-frame-parameter nil 'buffer-predicate
                                     (lambda (buf) (memq buf members)))
                (dotimes (_ (abs n))
                  (if (< n 0) (previous-buffer 1 t) (next-buffer 1 t))))
            (set-frame-parameter nil 'buffer-predicate pred)))))))

  (defun my/next-buffer (&optional n)
    "Switch N buffers forward in the current workspace."
    (interactive "p")
    (my/switch-buffer (or n 1)))

  (defun my/previous-buffer (&optional n)
    "Switch N buffers backward in the current workspace."
    (interactive "p")
    (my/switch-buffer (- (or n 1))))

  (defun add-buffer-to-workspace (buffer &optional workspace)
    "Add BUFFER to WORKSPACE, a perspective object (default: the current one)."
    (when (and persp-mode (buffer-live-p buffer))
      (let ((persp (or workspace (persp-get-current))))
        (when (perspective-p persp)
          (persp-add-buffer buffer persp nil nil)))))

  (defun next-workspace-id ()
    "Return the next unused numeric workspace id."
    (or (cl-loop for name in (list-workspace-names)
                 when (string-match-p "\\`#[0-9]+\\'" name)
                 maximize (string-to-number (substring name 1)) into max
                 finally return (and max (1+ max)))
        1))

  ;;; Display

  (defface workspace-tab-selected-face '((t :inherit highlight))
    "Face for the current workspace in `display'."
    :group 'persp-mode)

  (defface workspace-tab-face '((t :inherit default))
    "Face for the other workspaces in `display'."
    :group 'persp-mode)

  (defun workspace-tabline (&optional names)
    "Return the workspace tab line for NAMES."
    (let ((names (or names (list-workspace-names)))
          (current (current-workspace-name)))
      (mapconcat
       #'identity
       (cl-loop for name in names
                for index from 1
                collect (propertize (format " [%d] %s " index name)
                                    'face (if (equal name current)
                                              'workspace-tab-selected-face
                                            'workspace-tab-face)))
       " ")))

  (defun workspace-message (text &optional type)
    "Show TEXT after the workspace tab line, styled according to TYPE."
    (message "%s%s"
             (workspace-tabline)
             (propertize (format " | %s" text)
                         'face (pcase type
                                 ('error 'error)
                                 ('warning 'warning)
                                 ('success 'success)
                                 (_ 'font-lock-comment-face)))))

  (defun display ()
    "Display the open workspaces in the echo area, like tabs."
    (interactive)
    (let ((message-log-max nil))
      (message "%s" (workspace-tabline))))

  ;;; Switching and lifecycle

  (defun switch-workspace (name &optional auto-create-p)
    "Switch to the workspace named NAME.
Create it when AUTO-CREATE-P is non-nil and it does not exist."
    (unless (workspace-exists-p name)
      (if auto-create-p
          ;; Go through the command so the new workspace starts blank
          ;; instead of inheriting the current buffer and window layout.
          (new name)
        (user-error "%s is not an available workspace" name)))
    (let ((old-name (current-workspace-name)))
      (unless (equal old-name name)
        (when (and (not (equal old-name persp-nil-name))
                   (workspace-exists-p old-name))
          (setq last-workspace old-name))
        (persp-frame-switch name)))
    (equal (current-workspace-name) name))

  (defun new (&optional name clone-p)
    "Create and switch to a new workspace named NAME.
NAME defaults to the next free numeric id. With CLONE-P, clone the
buffers and window layout of the current workspace."
    (interactive (list nil current-prefix-arg))
    (unless name
      (setq name (format "#%s" (next-workspace-id))))
    (when (workspace-exists-p name)
      (user-error "A workspace named '%s' already exists" name))
    (if clone-p
        (progn
          (persp-copy name 'no-switch)
          (switch-workspace name))
      (let ((persp (persp-add-new name)))
        (switch-workspace name)
        ;; Doom starts a blank workspace in a single window on the fallback
        ;; buffer instead of keeping the previous workspace's layout.
        (delete-other-windows)
        (let ((scratch (get-buffer-create "*scratch*")))
          (switch-to-buffer scratch)
          (persp-add-buffer scratch persp))
        ;; No need to set `persp-window-conf' here: persp-mode saves the
        ;; window configuration when the workspace is deactivated, which
        ;; captures this single-window scratch layout.
        ))
    (display)
    name)

  (defun new-named (name)
    "Create and switch to a new workspace called NAME."
    (interactive "sWorkspace name: ")
    (new name))

  (defun rename (name new-name)
    "Rename the workspace NAME to NEW-NAME."
    (interactive
     (let ((current (current-workspace-name)))
       (list current
             (read-string (format "Rename workspace '%s' to: " current)
                          current))))
    (when (equal name persp-nil-name)
      (user-error "Can't rename the '%s' workspace" persp-nil-name))
    (when (equal name new-name)
      (user-error "Workspace is already named '%s'" name))
    (when (workspace-exists-p new-name)
      (user-error "A workspace named '%s' already exists" new-name))
    (let ((old (persp-rename new-name (find-workspace name))))
      (unless old
        (user-error "Couldn't rename workspace '%s'" name))
      (when (equal last-workspace name)
        (setq last-workspace new-name))
      (workspace-message (format "Renamed '%s' to '%s'" old new-name)
                             'success)
      old))

  (defun kill (workspace &optional inhibit-kill-p)
    "Kill WORKSPACE, a name or a perspective object.
When INHIBIT-KILL-P is non-nil, leave its buffers alive."
    (interactive
     (list (if current-prefix-arg
               (completing-read "Kill workspace: " (list-workspace-names))
             (current-workspace-name))))
    (let* ((name (if (stringp workspace)
                     workspace
                   (persp-name workspace)))
           (current (current-workspace-name))
           (names (list-workspace-names))
           (was-current (equal name current))
           (dest (or (and (not (equal last-workspace name))
                          (workspace-exists-p last-workspace)
                          last-workspace)
                     (car (remove name names))
                     main-workspace)))
      (when (equal name persp-nil-name)
        (user-error "Can't kill the '%s' workspace" persp-nil-name))
      (unless (member name names)
        (user-error "Workspace '%s' doesn't exist" name))
      (when (and was-current (null (cdr names)))
        (user-error "Can't delete the last workspace"))
      (persp-kill name inhibit-kill-p)
      ;; `persp-kill' moves frames that showed NAME to the nil perspective;
      ;; switch them to a live workspace explicitly.
      (when (equal last-workspace name)
        (setq last-workspace dest))
      (when was-current
        (switch-workspace dest t))
      (workspace-message (format "Deleted '%s' workspace" name)
                             'success)))

  (defun other ()
    "Switch to the previously active workspace."
    (interactive)
    (let ((current (current-workspace-name)))
      (switch-workspace
       (or (and (not (equal last-workspace current))
                (workspace-exists-p last-workspace)
                last-workspace)
           (car (remove current (list-workspace-names)))
           main-workspace)
       t)))

  (defun cycle (n)
    "Cycle N workspaces to the right; a negative N cycles to the left."
    (interactive (list 1))
    (let* ((names (list-workspace-names))
           (current (current-workspace-name))
           (count (length names)))
      (when (equal current persp-nil-name)
        (switch-workspace main-workspace t)
        (setq names (list-workspace-names)
              current main-workspace
              count (length names)))
      (when (< count 2)
        (user-error "No other workspaces"))
      (let* ((index (or (cl-position current names :test #'equal) 0))
             (dest (nth (% (+ index n count) count) names)))
        (switch-workspace dest)
        (display))))

  (defun switch-right (&optional n)
    "Cycle N workspaces to the right (default: 1)."
    (interactive "p")
    (cycle (or n 1)))

  (defun switch-left (&optional n)
    "Cycle N workspaces to the left (default: 1)."
    (interactive "p")
    (cycle (- (or n 1))))

  (defun switch-to (index)
    "Switch to the workspace at INDEX, counted from zero."
    (interactive
     (list (or current-prefix-arg
               (completing-read "Switch to workspace: "
                                (list-workspace-names)))))
    (when (and (stringp index)
               (string-match-p "\\`[0-9]+\\'" index))
      (setq index (string-to-number index)))
    (let ((names (list-workspace-names)))
      (cond
       ((numberp index)
        (let ((dest (nth index names)))
          (unless dest
            (user-error "No workspace at #%s" (1+ index)))
          (switch-workspace dest)))
       ((stringp index)
        (switch-workspace index t))
       (t
        (user-error "Not a valid index: %s" index)))))

  (dotimes (i 9)
    (defalias (intern (format "switch-to-%d" i))
      (lambda () (interactive) (switch-to i))
      (format "Switch to workspace #%d." (1+ i))))

  (defun switch-to-final ()
    "Switch to the last workspace in the workspace list."
    (interactive)
    (switch-workspace (car (last (list-workspace-names)))))

  ;;; Persistence

  (defun workspaces-data-file-path ()
    "Return the file where explicitly saved workspaces are stored."
    (expand-file-name workspaces-data-file persp-save-dir))

  (defun session-file-path ()
    "Return the file used by the session save/load commands."
    (expand-file-name persp-auto-save-fname persp-save-dir))

  (defun save (name)
    "Save the open workspace NAME so `open' can restore it."
    (interactive
     (list (if current-prefix-arg
               (current-workspace-name)
             (completing-read "Save workspace: "
                              (list-workspace-names)))))
    (unless (workspace-exists-p name)
      (user-error "'%s' is not an open workspace" name))
    (let ((file (workspaces-data-file-path)))
      (persp-save-to-file-by-names file *persp-hash* (list name) t)
      (if (member name (persp-list-persp-names-in-file file))
          (workspace-message (format "'%s' workspace saved" name) 'success)
        (user-error "Couldn't save workspace '%s'" name))))

  (defun open (name)
    "Load the saved workspace NAME and switch to it."
    (interactive
     (list (completing-read
            "Load workspace: "
            (persp-list-persp-names-in-file (workspaces-data-file-path)))))
    (when (workspace-exists-p name)
      (user-error "A workspace named '%s' already exists" name))
    (persp-load-from-file-by-names (workspaces-data-file-path)
                                   *persp-hash* (list name))
    (unless (workspace-exists-p name)
      (user-error "Couldn't load workspace '%s'" name))
    (switch-workspace name)
    (workspace-message (format "Loaded '%s' workspace" name) 'success))

  (defun discard (name)
    "Delete the saved workspace NAME from the workspaces data file."
    (interactive
     (list (completing-read
            "Delete saved workspace: "
            (persp-list-persp-names-in-file (workspaces-data-file-path)))))
    (let* ((file (workspaces-data-file-path))
           (savelist (persp-savelist-from-savefile file)))
      (unless (and savelist
                   (member name (persp-list-persp-names-in-file file savelist)))
        (user-error "Couldn't find saved workspace '%s'" name))
      (let* ((dispatch (persp-dispatch-loadf-version
                        'persp-names-from-savelist savelist))
             (version (nth 2 dispatch))
             (saved-workspaces (nth 1 dispatch))
             (remaining (cl-remove-if
                         (lambda (saved)
                           (equal name (persp-name-from-savelist-0 saved)))
                         saved-workspaces)))
        (if remaining
            (persp-savelist-to-file
             (if (equal version "0")
                 remaining
               (cons (car savelist) remaining))
             file)
          (delete-file file))
        (workspace-message
         (format "Deleted saved '%s' workspace" name) 'success))))

  (defun save-session ()
    "Save every workspace and its window layout to the session file."
    (interactive)
    (let ((file (session-file-path)))
      (persp-save-state-to-file file *persp-hash* nil)
      (workspace-message (format "Session saved to %s" file) 'success)))

  (defun load-session ()
    "Restore the workspaces and window layouts from the session file."
    (interactive)
    (let ((file (session-file-path)))
      (unless (file-exists-p file)
        (user-error "No session file at %s" file))
      (persp-load-state-from-file file *persp-hash*)
      (ensure-main-workspace)
      (workspace-message (format "Session restored from %s" file)
                             'success)))

  (defun restore-last-session ()
    "Restore the last auto-saved session."
    (interactive)
    (load-session))

  (defun kill-session ()
    "Kill every workspace and its buffers, then start a fresh `main'."
    (interactive)
    (let ((persp-autokill-buffer-on-remove t))
      (dolist (name (list-workspace-names))
        (ignore-errors (persp-kill name)))
      (switch-workspace main-workspace t)
      (dolist (buffer (buffer-list))
        (when (and (not (eq buffer (current-buffer)))
                   (not (string-prefix-p " " (buffer-name buffer))))
          (ignore-errors (kill-buffer buffer)))))
    (workspace-message "Session cleared" 'success))

  ;;; Workspace-aware buffer list

  (defun buffers ()
    "Open ibuffer restricted to the buffers in the current workspace."
    (interactive)
    (let ((names (mapcar #'buffer-name (workspace-buffers))))
      (ibuffer nil (format "*Ibuffer: %s*" (current-workspace-name))
               (list (cons 'predicate `(member (buffer-name) ',names))))))

  ;;; Startup and session hooks

  (defun ensure-main-workspace (&rest _)
    "Create `main-workspace', hide the nil workspace and move frames to it."
    (when persp-mode
      ;; `persp-nil-hidden' is obsolete since persp-mode 3.9.1. Hide the
      ;; nil workspace through the struct slot so it stays out of prompts.
      (setf (persp-hidden persp-nil-persp) t)
      (unless (workspace-exists-p main-workspace)
        (persp-add-new main-workspace))
      (let ((target (or (and (workspace-exists-p main-workspace)
                             main-workspace)
                        (car (list-workspace-names)))))
        (when target
          (dolist (frame (persp-frame-list-without-daemon))
            (when (string= (persp-name (persp-of-frame frame)) persp-nil-name)
              (persp-frame-switch target frame)
              ;; Keep the visible buffer inside the workspace so
              ;; `buffer-list' and buffer switching agree with the frame.
              (let ((persp (find-workspace target))
                    (buffer (window-buffer (frame-selected-window frame))))
                (when (and persp
                           (buffer-live-p buffer)
                           (not (persp-contain-buffer-p buffer persp)))
                  (persp-add-buffer buffer persp nil nil)))))))))

  (add-hook 'persp-mode-hook #'ensure-main-workspace)
  (add-hook 'persp-after-load-state-functions #'ensure-main-workspace)

  ;; To save its state, persp creates a temporary invisible frame and
  ;; calls persp-delete-other-windows on it while the selected window is
  ;; still in the main frame. Emacs 30 made `delete-other-windows' signal
  ;; when ROOT is not an ancestor of the selected window, and persp
  ;; catches it and complains on every save:
  ;;   [persp-mode] Warning: Can not delete-other-windows --
  ;;   (error "Specified root is not an ancestor of specified window")
  ;; persp ends up returning nil for that case anyway, so skipping the
  ;; call up front is behaviourally identical and only removes the noise.
  ;; NOTE: advice-add, not defadvice: defadvice is broken in Emacs 31.1
  ;; ("Eager macro-expansion failure: (wrong-type-argument listp :around)").
  (defun my/persp-delete-other-windows--around (orig-fun &optional frame win)
    "Skip ORIG-FUN when the target window lives in another frame."
    (let ((target (or (and (window-live-p win) (window-frame win))
                      frame)))
      (if (and target
               (frame-live-p target)
               (not (eq target (selected-frame))))
          nil
        (funcall orig-fun frame win))))

  (advice-add 'persp-delete-other-windows :around
              #'my/persp-delete-other-windows--around)

  ;; `persp-set-frame-buffer-predicate' caches the workspace's buffer list
  ;; and only clears that cache from a two-second timer. Until the timer
  ;; fires, the frame's `buffer-predicate' rejects buffers visited in the
  ;; meantime and `other-buffer' (used by `bury-buffer', `kill-this-buffer'
  ;; and the built-in `next-buffer') falls back to a stale buffer. Drop the
  ;; cache as soon as the buffer list changes instead.
  (add-hook 'buffer-list-update-hook
            (lambda () (setq persp-frame-buffer-predicate-buffer-list-cache nil)))

  (persp-mode +1))

;;; ### Workspace key bindings

;; Doom's `SPC TAB' workspace map and the workspace-aware buffer switches.
;; `my-leader' is defined in keys.el, which loads before this file.
(my-leader
 '(("TAB TAB" . display)
   ("TAB ."   . switch-to)
   ("TAB `"   . other)
   ("TAB n"   . new)
   ("TAB N"   . new-named)
   ("TAB l"   . open)
   ("TAB s"   . save)
   ("TAB x"   . kill-session)
   ("TAB d"   . kill)
   ("TAB D"   . discard)
   ("TAB r"   . rename)
   ("TAB R"   . restore-last-session)
   ("TAB ]"   . switch-right)
   ("TAB ["   . switch-left)
   ("TAB 1"   . switch-to-0)
   ("TAB 2"   . switch-to-1)
   ("TAB 3"   . switch-to-2)
   ("TAB 4"   . switch-to-3)
   ("TAB 5"   . switch-to-4)
   ("TAB 6"   . switch-to-5)
   ("TAB 7"   . switch-to-6)
   ("TAB 8"   . switch-to-7)
   ("TAB 9"   . switch-to-8)
   ("TAB 0"   . switch-to-final)
   (","       . persp-switch-to-buffer)
   ("<"       . switch-to-buffer)
   ("q s"     . save-session)
   ("q l"     . load-session)))

;; Doom binds these in normal state only; M-<digit> is global so it also
;; works from other Evil states and from the minibuffer's parent maps.
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-t") #'new)
  (define-key evil-normal-state-map (kbd "C-S-t") #'display))

(dotimes (i 9)
  (global-set-key (kbd (format "M-%d" (1+ i)))
                  (intern (format "switch-to-%d" i))))
(global-set-key (kbd "M-0") #'switch-to-final)

;; Ported from Doom's config.el: ensure a trailing newline in buffers that
;; live in the persisted window configuration directory. Doom used
;; defadvice!; advice-add is the plain-Elisp equivalent.
;;
;; `basic-save-buffer' may be called with a buffer argument, so the advice
;; has to accept one; `file-name-directory' always returns a trailing
;; slash, hence file-name-as-directory on the comparison.
(defun my-persp-autosave--add-breakline (&rest _args)
  "Append a newline to persisted buffers before they are saved."
  (let ((file (buffer-file-name)))
    (when (and file
               (/= (point-max) (point-min))
               (/= (char-after (1- (point-max))) ?\n)
               (string= (file-name-directory file)
                        (file-name-as-directory persp-save-dir)))
      (save-excursion
        (goto-char (point-max))
        (insert ?\n)))))

(advice-add 'basic-save-buffer :before #'my-persp-autosave--add-breakline)

;;; ### Indentation guides

;; Doom's (indent-guides): on in code buffers. Loaded after the theme
;; above, otherwise it cannot derive its faces. GUI only: face
;; auto-derivation needs a real background, which terminals (and batch
;; runs) don't provide.
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method (if (display-graphic-p) 'bitmap 'character))
  (highlight-indent-guides-responsive 'top)
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (when (display-graphic-p)
                (highlight-indent-guides-mode 1)))))

;;; ### Line numbers and cursor

;; Ported from Doom: relative line numbers everywhere, with the ordinary
;; per-major-mode exceptions below.
(setq-default display-line-numbers-type 'relative
              display-line-numbers t)
(global-display-line-numbers-mode +1)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;; Cursor shape and color per Evil state, gruvbox-tinted like Doom.
(with-eval-after-load 'evil
  (setq evil-normal-state-cursor '(box "#b8bb26")
        evil-insert-state-cursor '(bar "#fb4934")
        evil-visual-state-cursor '(hbar "#fabd2f")
        evil-replace-state-cursor '(hbar "#fb4934")
        evil-motion-state-cursor '(box "#83a598")
        evil-emacs-state-cursor '(box "#d3869b")
        evil-operator-state-cursor '(hbar "#b8bb26")))

;;; ### Doom eye-candy modules

;; hl-todo: TODO/FIXME/NOTE screaming in code, as in Doom.
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

;; vi-tilde-fringe: Vim-like ~ past end of buffer, as in Doom.
(use-package vi-tilde-fringe
  :config
  (global-vi-tilde-fringe-mode 1))

;; vc-gutter +pretty: git marks in the fringe, refreshed with magit.
;; :demand so the global mode is on from startup, like Doom.
(use-package diff-hl
  :demand t
  :hook (dired-mode . diff-hl-dired-mode)
  :config
  (global-diff-hl-mode 1)
  (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; solaire-mode: Doom's depth cue, dimming sidebars and popups.
(use-package solaire-mode
  :config
  (solaire-global-mode 1))

;; rainbow-delimiters in code, as Doom's doom module does.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Doom's +org provides org-hide-leading-stars with level 4.
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local org-startup-indented t)
            (when (fboundp 'org-hide-leading-stars)
              (org-hide-leading-stars 4))))

;; Tildes in the fringe past end-of-buffer. vi-frm is no longer published on
;; any archive; display-buffer-alist is what Doom's doom-first-input-hook
;; tweak used, so keep that behaviour.
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (boundp 'display-buffer-alist)
                       (assoc 'mode display-buffer-alist))
              (setcdr (assoc 'mode display-buffer-alist) 'other))))

(blink-cursor-mode 1)

;;; ### Scroll and wrap

(setq scroll-margin 8
      scroll-conservatively 65
      smooth-scroll t)

;; Doom's word-wrap: visual line mode globally, with long prose buffers
;; wrapped at a readable width.
(use-package visual-fill-column
  :custom
  (visual-fill-column-width 120)
  (visual-fill-column-center-text t)
  :config
  (global-visual-line-mode t))

;;; ### Splash screen

(when (display-graphic-p)
  (setq fancy-splash-image
        (expand-file-name "images/gnu_color.png" my/assets-dir)))

;;; ### Dashboard

;; Doom's doom-dashboard is no longer published on any archive, but it was
;; only a layer over the `dashboard' package, so the layout is rebuilt
;; here with the sections Doom showed: banner, greeting, projects,
;; bookmarks, recent files and version.
;; NOTE: this version of dashboard composes widgets through
;; `dashboard-startupify-list'; the old dashboard-set-* helpers are gone.
(use-package dashboard
  :custom
  ;; Doom centred the whole dashboard; section shortcut letters stay
  ;; visible so every section has an easy access key (p/m/r/a...).
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-items '((projects . 5) (bookmarks . 5) (agenda . 8) (recents . 5)))
  (dashboard-items-default-length 8)
  ;; Doom's greeting, from its config.el.
  (dashboard-banner-logo-title
   (format "Welcome Home, %s." (or (getenv "USER") (user-login-name))))
  ;; Doom's banner image.
  (dashboard-startup-banner
   (expand-file-name "images/gnu_color.png" my/assets-dir))
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-navigator
     dashboard-insert-newline
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-newline
     dashboard-insert-footer))
  ;; Every section shows its icon before the name (nerd-icons when
  ;; available, all-the-icons otherwise; hidden in terminal).
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  ;; Section shortcut letters are always shown (dashboard only shows
  ;; them — and only defines the jump keys — for non-empty sections).
  ;; The overlay renames below keep the underlying "Name:" text intact,
  ;; so section search/jump keeps working.
  (dashboard-heading-shorcut-format "")
  (dashboard-item-names
   '(("Projects:" . "Projects: (p)")
     ("Agenda for today:" . "Agenda for today: (a)")
     ("Agenda for the coming week:" . "Agenda for the coming week: (a)")
     ("Bookmarks:" . "Bookmarks: (m)")
     ("Recent Files:" . "Recent Files: (r)")))
  :config
  (dashboard-setup-startup-hook)
  ;; dashboard switches to its buffer without registering it with
  ;; persp-mode, so it would only belong to the hidden nil workspace. Doom
  ;; keeps it inside the main workspace; do the same for the current one so
  ;; it survives workspace switches and appears in buffer lists.
  (defun add-dashboard-to-workspace (&rest _)
    "Add the dashboard buffer to the current workspace."
    (add-buffer-to-workspace (get-buffer dashboard-buffer-name)))
  (add-hook 'dashboard-after-initialize-hook #'add-dashboard-to-workspace)
  (advice-add 'dashboard-open :after #'add-dashboard-to-workspace)
  ;; Navigator row with note actions (Doom's dashboard had one too).
  ;; Buttons activate with RET (or mouse); each is ("" title help action).
  (setq dashboard-navigator-buttons
        `((("" "New note" "Create or open a note"
             ,(lambda (&rest _) (call-interactively #'org-roam-node-find)))
           ("" "Capture" "Quick capture (SPC j c)"
             ,(lambda (&rest _) (call-interactively #'org-capture)))
           ("" "Journal" "Today's journal entry"
             ,(lambda (&rest _) (call-interactively #'org-journal-new-entry)))
           ("" "Search" "Search all notes"
             ,(lambda (&rest _) (call-interactively #'deft)))
           ("" "Feeds" "Read RSS feeds"
             ,(lambda (&rest _) (call-interactively #'elfeed))))))
  ;; Jump commands that work even when a section is empty (dashboard only
  ;; generates its own when the section has items).
  (defun my/dashboard-jump-to-section (label)
    "Move point to the dashboard section headed LABEL."
    (goto-char (point-min))
    (if (search-forward label nil t)
        (progn (forward-line 1) (back-to-indentation))
      (goto-char (point-min))))
  (defun my/dashboard-jump-to-projects ()
    "Jump to the dashboard Projects section."
    (interactive)
    (my/dashboard-jump-to-section "Projects:"))
  (defun my/dashboard-jump-to-bookmarks ()
    "Jump to the dashboard Bookmarks section."
    (interactive)
    (my/dashboard-jump-to-section "Bookmarks:"))
  (defun my/dashboard-jump-to-agenda ()
    "Jump to the dashboard Agenda section (week or day view)."
    (interactive)
    (goto-char (point-min))
    (if (or (search-forward "Agenda for the coming week:" nil t)
            (progn (goto-char (point-min))
                   (search-forward "Agenda for today:" nil t)))
        (progn (forward-line 1) (back-to-indentation))
      (goto-char (point-min))))
  (defun my/dashboard-jump-to-recents ()
    "Jump to the dashboard Recent Files section."
    (interactive)
    (my/dashboard-jump-to-section "Recent Files:"))
  ;; Plain bindings cover non-Evil states; Evil normal-state bindings win
  ;; over evil's own keys (e.g. "p" pastes) inside the dashboard.
  (define-key dashboard-mode-map (kbd "p") #'my/dashboard-jump-to-projects)
  (define-key dashboard-mode-map (kbd "a") #'my/dashboard-jump-to-agenda)
  (define-key dashboard-mode-map (kbd "m") #'my/dashboard-jump-to-bookmarks)
  (define-key dashboard-mode-map (kbd "r") #'my/dashboard-jump-to-recents)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      (kbd "p") #'my/dashboard-jump-to-projects
      (kbd "a") #'my/dashboard-jump-to-agenda
      (kbd "m") #'my/dashboard-jump-to-bookmarks
      (kbd "r") #'my/dashboard-jump-to-recents
      (kbd "RET") #'my/dashboard-press-button))
  ;; RET activates widgets (navigator buttons, section items), which
  ;; Evil's normal-state RET would otherwise swallow.
  (defun my/dashboard-press-button ()
    "Press the widget button at point, if any."
    (interactive)
    (let ((w (widget-at (point))))
      (if w
          (widget-button-press (point))
        (forward-line 1))))
  (define-key dashboard-mode-map (kbd "RET") #'my/dashboard-press-button)
  ;; TAB cycles through buttons/items so RET can press them; in a
  ;; read-only dashboard Evil's jump list is useless anyway.
  (define-key dashboard-mode-map (kbd "TAB") #'widget-forward)
  (define-key dashboard-mode-map (kbd "<backtab>") #'widget-backward)
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      (kbd "RET") #'my/dashboard-press-button
      (kbd "TAB") #'widget-forward
      (kbd "<backtab>") #'widget-backward)))

;; Doom showed the dashboard instead of *scratch* on startup.
(setq inhibit-startup-screen t
      inhibit-splash-screen t
      initial-buffer-choice nil)
;; Ensure the dashboard takes focus after persp-mode/workspaces have had a
;; chance to (attempt to) restore their saved state, and leave point on
;; the Projects section (shortcut "p", generated as dashboard-jump-to-projects
;; when the section renders).
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 0.3 nil
                            (lambda ()
                              (dashboard-initialize)
                              (let ((buf (get-buffer "*dashboard*")))
                                (when (buffer-live-p buf)
                                  (with-current-buffer buf
                                    (my/dashboard-jump-to-projects)))))))
          t)

;;; ### Window borders and transparency

;; Remove the menu bar and the tool bar: with a frameless window (see
;; `undecorated' in 00-defaults.el) they are the only bars left at the
;; top of the frame. This is Doom's remove-top-bar module.
;;
;; NOTE: these must be 0, never nil. With nil the window accessors such
;; as `window-right-divider-width' return nil, and doom-modeline subtracts
;; them unguarded:
;;   (- (window-pixel-width) (window-scroll-bar-width)
;;      (window-right-divider-width) ... (car (window-fringes)))
;; which fails with "Wrong type argument: integerp, nil" and takes the
;; whole mode line down. Zero looks the same on screen and stays an
;; integer.
(setq internal-border-width 0
      internal-border-width-horizontal 0
      vertical-border-width 0
      right-divider-width 0)

(defun my/hide-chrome ()
  "Hide the menu bar, the tool bar and the internal borders."
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1)
    (when (display-graphic-p)
      ;; tool-bar-mode only affects the first frame reliably.
      (dolist (frame (frame-list))
        (set-frame-parameter frame 'menu-bar-lines 0)
        (set-frame-parameter frame 'tool-bar-lines 0)
        (set-frame-parameter frame 'internal-border-width 0)
        (set-frame-parameter frame 'vertical-border-width 0)))))

(add-hook 'after-make-frame-functions #'my/hide-chrome)
(add-hook 'emacs-startup-hook #'my/hide-chrome)

;; Transparent terminal: keep the default face background unset in TTY
;; frames so the compositor shows through.
;; In a terminal, leave the default background unspecified so the
;; terminal emulator's own background shows through.
(unless (display-graphic-p)
  (set-face-background 'default nil))

;; Alpha 94/97, matching the previous Doom config; uncomment to restore.
;; (add-to-list 'default-frame-alist '(alpha . (94 . 97)))

;;; ### Comment and indentation defaults

(setq comment-tabs t)
(setq-default tab-width 2
              indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq tab-always-indent nil)


;;; my-ui.el ends here
(setq nerd-icons-scale-factor 0.95
      nerd-icons-default-adjust -0.2)

(provide 'my-ui)
;;; my-ui.el ends here
