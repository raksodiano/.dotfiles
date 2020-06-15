;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package js2-mode
  :straight t
  :config
  (add-hook 'js-mode-hook 'tern-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js-format
  :straight t
  :after js2-mode
  :config (add-hook 'js2-mode-hook
                    (lambda()
                      (js-format-setup "standard"))))

(use-package tern
  :straight t
  :config
  ;; Force restart of tern in new projects
  ;; $ M-x delete-tern-process
  (setq tern-command '("/usr/bin/tern" "--no-port-file"))
  (defun delete-tern-process ()
    "Force restart of tern in new project."
    (interactive)
    (delete-process "Tern")))

(use-package company-tern
  :straight t
  :init
  (defun cfg:js-mode-hook ()
    (add-to-list 'company-backends 'company-tern))

  (add-hook 'js-mode-hook 'cfg:js-mode-hook))

(use-package json-mode
  :mode "\\.json\\'"
  :straight t)

(use-package typescript-mode
  :hook (typescript-mode .
                         (lambda ()
                           (add-to-list
                            (make-local-variable
                             'grep-find-ignored-directories) "build")
                           (electric-indent-mode -1)))
  :mode (rx ".ts" (? "x") string-end))

(use-package vue-mode
  :init
  (setq mmm-submode-decoration-level 2))

;; Indium se conecta con una pestaña del navegador web o un proceso de NodeJS
;; y provee varias características para el desarrollo en JavaScript
;; (use-package indium ;; https://indium.readthedocs.io/en/latest/setup.html
;;   :config
;;   (add-hook 'js-mode-hook #'indium-interaction-mode))

(provide 'config)
;;; config.el ends here
