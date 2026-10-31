;;; secrets.el -*- lexical-binding: t; -*-
;;; secrets.el --- Central credential directory (~/.secrets)

;;; Commentary:
;; Management shortcuts for the central credential directory. Every
;; credential used inside Emacs (gptel, agent-shell, ERC, jabber, ...)
;; is looked up in ~/.secrets/ through auth-source, which early-init.el
;; registers as `auth-sources'. This module only adds shortcuts to
;; browse the directory and append entries to it.
;;
;; Layout: one file per service (llm, irc, xmpp, ...); the file name is
;; irrelevant, the `machine' field selects the entry. Files ending in
;; .gpg are encrypted by EPA when saved and decrypted on read.

;;; Code:

(defconst my/secrets-dir (expand-file-name ".secrets" "~")
  "Directory holding every credential used inside Emacs.")

(defun my/secrets-open ()
  "Browse the credential directory."
  (interactive)
  (dired my/secrets-dir))

(defun my/secrets-add (service host user)
  "Append a credential for HOST and USER to SERVICE in `my/secrets-dir'.
SERVICE is a file name inside the directory (llm, irc, xmpp, ...);
typing a name that does not exist yet creates it. The password is
prompted for without echoing. Saving a file whose name ends in .gpg
encrypts it via EPA."
  (interactive
   (let* ((files (directory-files my/secrets-dir nil "^[^.]"))
          (service (completing-read "Service file (existing or new): " files))
          (host (read-string "Host (machine): "))
          (user (read-string "Login: ")))
     (list service host user)))
  (let ((path (expand-file-name service my/secrets-dir))
        (secret (read-passwd (format "Password for %s@%s: " user host))))
    (with-temp-buffer
      (when (file-readable-p path)
        (insert-file-contents path))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "machine %s login %s password %s\n" host user secret))
      (write-region (point-min) (point-max) path))
    ;; `auth-sources' is computed once at startup (early-init.el), so a
    ;; file created now would only be seen after a restart. Register it
    ;; immediately and drop any cached lookup for this host, so the key
    ;; is usable without restarting.
    (add-to-list 'auth-sources path)
    (auth-source-forget host)
    (message "Credential for %s@%s added to %s" user host path)))

(my-leader
 '(("o C" . my/secrets-open)
   ("o A" . my/secrets-add)))

(provide 'my-secrets)
;;; secrets.el ends here
