;;; agent-shell.el -*- lexical-binding: t; -*-
;;; agent-shell.el --- Coding agents via ACP (agent-shell)

;;; Commentary:
;; Brings ACP coding agents (Claude Code, Codex, Gemini CLI, Goose, Qwen
;; Code, ...) into Emacs as native buffers, no terminal emulation.
;;
;; CREDENTIALS: every API key comes from ~/.secrets/ through
;; auth-source (see early-init.el and modules/tools/secrets.el) --
;; nothing is ever stored in this repository. The providers wired below
;; authenticate with `my/agent-shell-secret', which looks the key up by
;; host; add entries with SPC o A. The agent CLI binary itself must
;; still be installed (see the upstream README).
;;
;; The `machine' hosts used:
;;   api.anthropic.com               Anthropic (Claude Code)
;;   api.openai.com                  OpenAI (Codex)
;;   generativelanguage.googleapis.com  Google (Gemini CLI)
;;   api.deepseek.com                DeepSeek
;;
;; DATA: by default agent-shell writes per-project data (transcripts,
;; screenshots, ...) to a .agent-shell/ directory in the project root,
;; which would pollute this repository when working on the config
;; itself. `my/agent-shell-dot-subdir' redirects it to XDG data instead.

;;; Code:

;; `string-remove-prefix' (used below) lives in subr-x, which is not
;; always loaded yet when this module runs.
(require 'subr-x)

(defun my/agent-shell-secret (host)
  "Return the API key stored for HOST in ~/.secrets, via auth-source.
Returns nil when no entry matches. See early-init.el for how the
directory is registered as `auth-sources'."
  (when-let* ((entry (car (auth-source-search :host host :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(use-package agent-shell
  :commands agent-shell
  :init
  (progn
    ;; Keep per-project agent data out of the project tree: store it
    ;; under XDG data (see my/emacs-data in early-init.el), one directory
    ;; per project, named after the flattened project path.
    (defun my/agent-shell-dot-subdir (subdir)
      "Absolute path for agent-shell SUBDIR, outside the project tree.
Data lands under XDG data in a directory per project, named after the
flattened project path (e.g. home-user-Workspace-myproject)."
      (let* ((root (expand-file-name "agent-shell" my/emacs-data))
             (project (replace-regexp-in-string
                       "/" "-" (string-remove-prefix
                                "/" (directory-file-name (agent-shell-cwd))))))
        (expand-file-name subdir (expand-file-name project root))))
    (setq agent-shell-dot-subdir-function #'my/agent-shell-dot-subdir))
  :config
  ;; Providers authenticate with keys from ~/.secrets (looked up when a
  ;; session starts, so a key added later is picked up on the next
  ;; shell). By design there is no fallback to the CLIs' own login
  ;; stores: without a matching entry in ~/.secrets the agent will not
  ;; authenticate -- add the key with SPC o A.
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (lambda () (my/agent-shell-secret "api.anthropic.com")))
        agent-shell-openai-authentication
        (agent-shell-openai-make-authentication
         :api-key (lambda () (my/agent-shell-secret "api.openai.com")))
        agent-shell-google-authentication
        (agent-shell-google-make-authentication
         :api-key (lambda () (my/agent-shell-secret
                               "generativelanguage.googleapis.com")))
        agent-shell-deepseek-authentication
        (agent-shell-deepseek-make-authentication
         :api-key (lambda () (my/agent-shell-secret "api.deepseek.com"))))
  ;; Same placement rule as the other AI chats: full frame when narrow,
  ;; otherwise a right side window at least 15% wider than Treemacs and
  ;; wide enough for the whole header line.
  (setq agent-shell-display-action '(my/ai-display-buffer)))

(my-leader
 '(("y S" . agent-shell)))

(provide 'my-agent-shell)
;;; agent-shell.el ends here
