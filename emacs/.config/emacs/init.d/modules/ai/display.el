;;; display.el --- Shared window policy for AI buffers  -*- lexical-binding: t; -*-

;;; Commentary:
;; AI chat buffers (Ellama, gptel, agent-shell) should never open in a
;; window too small to read, and should not display line numbers.  On a
;; narrow frame they take the whole frame; on a wide frame they open in
;; a right side window that is at least 15% wider than Treemacs and is
;; widened further when necessary to show the whole header line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar agent-shell-header-style)

(defgroup my-ai nil
  "AI buffers and clients."
  :group 'applications)

(defcustom my/ai-side-window-ratio 1.15
  "Width of AI side windows as a multiple of the Treemacs width."
  :type 'number
  :group 'my-ai)

(defcustom my/ai-side-window-min-main-width 60
  "Columns to leave for the main window when an AI side window is open."
  :type 'integer
  :group 'my-ai)

(defcustom my/ai-side-window-min-width 60
  "Minimum width in columns for an AI side window.
Wide enough for the header-line buttons of the AI clients.  The
window grows further when a client needs more room."
  :type 'integer
  :group 'my-ai)

(defcustom my/ai-side-window-fallback-treemacs-width 35
  "Treemacs width used when Treemacs is not loaded.
Keep this in sync with `treemacs-width'."
  :type 'integer
  :group 'my-ai)

(defun my/ai--treemacs-window ()
  "Return the visible Treemacs window in the selected frame, if any."
  (when (fboundp 'treemacs-get-local-window)
    (ignore-errors (treemacs-get-local-window))))

(defun my/ai--treemacs-width ()
  "Return the width in columns of Treemacs.
When Treemacs is not visible, use its configured width, or
`my/ai-side-window-fallback-treemacs-width' if it is not loaded."
  (if-let* ((window (my/ai--treemacs-window)))
      (window-width window)
    (if (boundp 'treemacs-width)
        (symbol-value 'treemacs-width)
      my/ai-side-window-fallback-treemacs-width)))

(defun my/ai-side-window-width ()
  "Return the base width in columns for an AI side window.
It is at least 15% wider than Treemacs and never narrower than
`my/ai-side-window-min-width'."
  (max my/ai-side-window-min-width
       (round (* my/ai-side-window-ratio (my/ai--treemacs-width)))))

(defun my/ai-gptel-header-width ()
  "Return the width needed by the current gptel header line, or nil.
The header has the backend/status on the left and the buttons
\(system prompt, context, tools, media, usage and model) aligned to
the right, so the whole line needs both parts side by side."
  (when (and (bound-and-true-p gptel-mode)
             (bound-and-true-p gptel-use-header-line)
             (boundp 'gptel--header-line-info)
             (consp gptel--header-line-info))
    (let ((rhs (ignore-errors
                 (eval (cadr gptel--header-line-info) t))))
      (when (stringp rhs)
        (+ (let ((backend (bound-and-true-p gptel-backend)))
             (if (and backend (fboundp 'gptel-backend-name))
                 (string-width (gptel-backend-name backend))
               0))
           ;; The status button can be longer than "Ready"
           ;; (" Waiting...", " Calling tool...", ...).
           (let ((status (and (consp header-line-format)
                              (nth 1 header-line-format))))
             (if (stringp status) (string-width status) 11))
           ;; Gap between the two header segments (2), the right
           ;; padding gptel reserves in text terminals (5), and one
           ;; spare column.
           8
           (string-width rhs))))))

(defun my/ai-agent-shell-header-width ()
  "Return the columns needed by the current agent-shell header, or nil.
Its graphical header is an SVG sized to the window, so the header
line itself does not reveal how wide the buttons are.  Render the
same header as text, without building its click menus, and leave
room for the icon and padding."
  (when (and (derived-mode-p 'agent-shell-mode)
             (not (memq (bound-and-true-p agent-shell-header-style)
                        '(nil none)))
             (boundp 'agent-shell--header-last-model)
             agent-shell--header-last-model
             (fboundp 'agent-shell--render-header-model-uncached))
    (let ((agent-shell-header-style 'text))
      (ignore-errors
        (cl-letf (((symbol-function 'agent-shell--mode-line-model-menu)
                   #'ignore)
                  ((symbol-function 'agent-shell--mode-line-mode-menu)
                   #'ignore)
                  ((symbol-function 'agent-shell--mode-line-thought-level-menu)
                   #'ignore))
          (+ 12 (string-width
                 (agent-shell--render-header-model-uncached
                  agent-shell--header-last-model))))))))

(defun my/ai-header-width (buffer)
  "Return the columns needed by BUFFER's header line, or nil.
gptel and agent-shell get precise measurements because their
headers are not plain header-line strings.  Other clients use the
formatted header line when it is available."
  (with-current-buffer buffer
    (or (my/ai-gptel-header-width)
        (my/ai-agent-shell-header-width)
        (when (and (boundp 'header-line-format)
                   header-line-format
                   (not (eq header-line-format t)))
          (let ((formatted (ignore-errors
                             (format-mode-line header-line-format
                                               nil nil (current-buffer)))))
            (when (and (stringp formatted)
                       (> (string-width formatted) 0))
              (string-width formatted)))))))

(defun my/ai-side-window-width-for-buffer (buffer)
  "Return the side-window width needed to show BUFFER's header line.
Never smaller than `my/ai-side-window-min-width' or the Treemacs
based width."
  (max (my/ai-side-window-width)
       (or (my/ai-header-width buffer) 0)))

(defun my/ai-side-window-fits-p (&optional width)
  "Return non-nil when a right-hand AI side window fits.
WIDTH defaults to `my/ai-side-window-width'.  Leave
`my/ai-side-window-min-main-width' columns for the main window,
plus the Treemacs width when it is visible."
  (let* ((width (or width (my/ai-side-window-width)))
         (treemacs-window (my/ai--treemacs-window))
         (treemacs-width (if treemacs-window
                             (window-width treemacs-window)
                           0)))
    (>= (frame-width)
        (+ treemacs-width
           width
           my/ai-side-window-min-main-width))))

(defun my/ai-display-buffer (buffer _alist)
  "Display BUFFER according to the AI window policy.
Use the full frame when it is too narrow for a side panel; otherwise
use a right side window wide enough for the header-line buttons."
  (let ((width (my/ai-side-window-width-for-buffer buffer)))
    (display-buffer
     buffer
     (if (my/ai-side-window-fits-p width)
         `((display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . ,width)
           (preserve-size . (t . nil))
           (body-function . select-window))
       '((display-buffer-full-frame)
         (body-function . select-window))))))

(defcustom my/ai-align-response-tables t
  "Whether AI responses keep tables aligned.
When non-nil, AI clients run the major mode's table alignment
function after inserting the LLM response."
  :type 'boolean
  :group 'my-ai)

(defun my/ai-align-tables-in-response (start end)
  "Align the AI response tables between START and END.
This runs in the buffer where the AI response was inserted."
  (when (and (numberp start) (numberp end) (< start end))
    (cond
     ((and (derived-mode-p 'org-mode) (fboundp 'org-at-table-p))
      (save-excursion
        (goto-char start)
        (while (and (< (point) end) (not (eobp)))
          (when (org-at-table-p)
            (org-table-align)
            (goto-char (org-table-end))
            (forward-line 1))))))
     ((and (derived-mode-p 'markdown-mode) (fboundp 'markdown-table-at-point-p))
      (save-excursion
        (goto-char start)
        (while (and (< (point) end) (not (eobp)))
          (when (markdown-table-at-point-p)
            (markdown-table-align)
            (goto-char (markdown-table-end))
            (forward-line 1)))))))

(defun my/ai-post-response-tables (start end)
  "Align tables inserted by an AI client.
This is meant for AI clients that run post-response hooks with the
buffer positions of the inserted response."
  (when (and my/ai-align-response-tables
             (numberp start) (numberp end)
             (< start end))
    (my/ai-align-tables-in-response start end)))

;;; Line numbers

(defun my/ai-mode-p ()
  "Return non-nil when the current buffer belongs to an AI client.
Covers the major modes of the AI packages as well as the minor
modes that turn an ordinary Org/Markdown/text buffer into a chat."
  (or (derived-mode-p 'gptel-context-buffer-mode
                      'ellama-blueprint-mode
                      'ellama-context-mode
                      'agent-shell-mode
                      'agent-shell-viewport-view-mode
                      'agent-shell-viewport-edit-mode
                      'agent-shell-diff-mode)
      (bound-and-true-p gptel-mode)
      (bound-and-true-p ellama-session-mode)))

(defun my/ai-line-numbers-off ()
  "Turn off line numbers in AI buffers.
Installed in `display-line-numbers-mode-hook', so it also overrides
`global-display-line-numbers-mode' when it tries to turn them on."
  (when (and (bound-and-true-p display-line-numbers-mode)
             (my/ai-mode-p))
    (display-line-numbers-mode -1)))

(add-hook 'display-line-numbers-mode-hook #'my/ai-line-numbers-off)
(add-hook 'gptel-mode-hook #'my/ai-line-numbers-off)
(add-hook 'ellama-session-mode-hook #'my/ai-line-numbers-off)

(defun my/ai-gptel-fit-window ()
  "Grow the selected gptel side window when its header line needs it.
The gptel header is dynamic (context, tools, usage, status, model),
so the panel may have to be stretched after the buffer was opened."
  (when (and (bound-and-true-p gptel-mode)
             (window-parameter (selected-window) 'window-side)
             (eq (window-buffer (selected-window)) (current-buffer)))
    (let ((needed (my/ai-gptel-header-width)))
      (when (and needed
                 (> needed (window-width))
                 (my/ai-side-window-fits-p needed))
        (ignore-errors
          (window-resize (selected-window)
                         (- needed (window-width)) t))))))

(defun my/ai-gptel-setup-fit-hook ()
  "Register `my/ai-gptel-fit-window' while the gptel buffer is active."
  (if (bound-and-true-p gptel-mode)
      (add-hook 'post-command-hook #'my/ai-gptel-fit-window nil t)
    (remove-hook 'post-command-hook #'my/ai-gptel-fit-window t)))

(add-hook 'gptel-mode-hook #'my/ai-gptel-setup-fit-hook)

(provide 'my-ai-display)
;;; display.el ends here


