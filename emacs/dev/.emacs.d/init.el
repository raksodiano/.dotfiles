;;; init.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; This config start here

;;; Code:
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs cargado en %s y listos para ser usado."
                     (format "%.2f segundos"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;
;; Load the constants
;;
(require 'core-constants (concat user-emacs-directory "core/core-constants.el"))
(require 'core-functions (concat user-emacs-directory "core/core-functions.el"))

(setq nsm-settings-file (concat cache-dir "network-security.data"))
(setq network-security-level 'high)

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defvar bootstrap-version)

(defvar straight-use-package-by-default t)
(setq straight-repository-branch "develop"
      straight-base-dir local-dir
      straight-check-for-modifications '(check-on-save-find-when-checking))

;;straight-vc-git-default-clone-depth 100)
;; This avoid that straight.el check for packages modifications on startup
;; when I modify a package I run manually straight-rebuild-package so this checking
;; on startup is not needed
(defvar straight-check-for-modifications nil)

;;
;; Install straight.el
;;
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" local-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight-x)

;;
;; Load the kernel
;;
(require 'core (concat user-emacs-directory "core/core.el"))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Restore defalut values after init."
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)

            (add-hook 'focus-out-hook 'garbage-collect)))

(provide 'init)
;;; init.el ends here
