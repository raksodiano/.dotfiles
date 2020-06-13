;;; core-packages.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(setq nsm-settings-file (concat cache-dir "network-security.data"))
(setq network-security-level 'medium)

;; Install use-package using straight
(straight-use-package 'use-package)

;; setup zsh as a default shell when is available
(let ((zsh-path (executable-find "zsh")))
  (when zsh-path
    (setq shell-file-name zsh-path)))

(setq package--init-file-ensured t
      package-user-dir (expand-file-name "elpa" packages-dir)
      package-enable-at-startup nil
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/"))

      package-archive-priorities '(("melpa" . 10)
                                   ("gnu" . 8)
                                   ("org" . 6)))

(defun rakso-update-one-package (package)
  "Actualiza un paquete PACKAGE"
  (when (package-installed-p package)
    (let* ((newest-pkg (car-safe (cdr (assq package package-archive-contents))))
           (new-ver (and newest-pkg (package-desc-version newest-pkg)))
           (builtin-pkg (cdr (assq package package--builtins)))
           (installed-pkg (car-safe (cdr (assq package package-alist))))
           (old-dir (and installed-pkg (package-desc-dir installed-pkg)))
           (old-ver (or (and installed-pkg (package-desc-version installed-pkg))
                        (and builtin-pkg (package--bi-desc-version builtin-pkg)))))
      (when (and new-ver (version-list-< old-ver new-ver))

        (condition-case nil
            ;; en caso de algún error tratando de bajar algún paquete, captura
            ;; el error para que no interfiera con la inicialización de Emacs
            (progn (package-install newest-pkg)
                   (message (format "Paquete «%s» actualizado de la versión %s a la versión %s"
                                    (package-desc-name newest-pkg) old-ver new-ver))))
        (when old-dir
          (delete-directory old-dir t))))))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-enable-at-startup nil
      package--init-file-ensured t)

(require 'bind-key)

(use-package validate
  :ensure t)

(use-package diminish
  :ensure t)

(diminish 'undo-tree-mode)
(diminish 'hs-minor-mode)
(diminish 'auto-revert-mode)

(use-package package-lint
  :ensure t)

(use-package hydra
  :ensure t)

(use-package speed-type
  :ensure t)

(use-package vlf-setup
  :straight vlf
  :init
  (setf vlf-application 'dont-ask))

(provide 'core-packages)
;;; core-packages.el ends here
