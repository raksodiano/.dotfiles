;;; straight.el --- Bootstrap straight.el from GitHub -*- lexical-binding: t; -*-

;;; Commentary:
;; straight.el is a package manager that installs packages from Git
;; repositories. This file only bootstraps it: on the first start it fetches
;; the official installer, which clones straight.el into
;; `straight-base-dir' (kept outside this stow-managed repository); on later
;; starts it just loads the generated bootstrap file.
;;
;; See `early-init.el' for the `straight-base-dir' setting.

;;; Code:

;; Make `bootstrap-version' special so the installer can pass it to the
;; child Emacs it starts while building straight itself.
(defvar bootstrap-version)

(let* ((base-dir (or (bound-and-true-p straight-base-dir)
                     user-emacs-directory))
       (bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el"
                          base-dir))
       (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    ;; A failed download must not abort startup: package.el still works.
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously
             (concat "https://raw.githubusercontent.com/radian-software/straight.el/"
                     "develop/install.el")
             'silent 'inhibit-cookies)
          ;; `install.el' ends with a single form that performs the bootstrap.
          (goto-char (point-max))
          (eval-print-last-sexp))
      (error
       (message "straight.el: bootstrap failed: %s"
                (error-message-string err)))))
  (when (file-exists-p bootstrap-file)
    (load bootstrap-file nil 'nomessage)))

(provide 'straight)
;;; straight.el ends here
