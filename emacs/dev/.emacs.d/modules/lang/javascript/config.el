;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf js2-mode
      :straight t
      :config
      (add-hook 'js-mode-hook 'tern-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(leaf js-format
      :straight t
      :after js2-mode
      :config (add-hook 'js2-mode-hook
                        (lambda()
                          (js-format-setup "standard"))))

(leaf tern
      :straight t
      :config
      ;; Force restart of tern in new projects
      ;; $ M-x delete-tern-process
      (setq tern-command '("/usr/bin/tern" "--no-port-file"))
      (defun delete-tern-process ()
        "Force restart of tern in new project."
        (interactive)
        (delete-process "Tern")))

(leaf company-tern
      :straight t
      :init
      (defun cfg:js-mode-hook ()
        (add-to-list 'company-backends 'company-tern))

      (add-hook 'js-mode-hook 'cfg:js-mode-hook))

(leaf json-mode
      :mode "\\.json\\'"
      :straight t)

(leaf typescript-mode
      :straight t
      :hook (typescript-mode .
                             (lambda ()
                               (add-to-list
                                (make-local-variable
                                 'grep-find-ignored-directories) "build")
                               (electric-indent-mode -1)))
      :mode (rx ".ts" (? "x") string-end))

(leaf vue-mode
      :straight t
      :init
      (setq mmm-submode-decoration-level 2))

;; Indium se conecta con una pestaña del navegador web o un proceso de NodeJS
;; y provee varias características para el desarrollo en JavaScript
;; (leaf indium ;; https://indium.readthedocs.io/en/latest/setup.html
;;   :config
;;   (add-hook 'js-mode-hook #'indium-interaction-mode))

(provide 'config)
;;; config.el ends here
