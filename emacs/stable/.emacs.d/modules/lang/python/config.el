;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(defun my/python-format-code ()
  (interactive)
  (if (executable-find "black")
      (python-black-format-buffer)
    (elpy-format-code)))

(use-package elpy
  :ensure t
  :diminish
  :after (python)
  :custom
  (elpy-shell-echo-input . nil)
  :config
  (elpy-enable)
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-rpc-python-command "python")
  (add-hook 'elpy-mode-hook
            (lambda ()
              (hs-minor-mode)
              (highlight-indentation-mode -1)))) ; Remove vertical line

(setq python-shell-completion-native-enable nil)

(use-package py-isort
  :ensure t
  :after (elpy)
  :init
  (defun my/sort-imports ()
    (interactive)
    (if (region-active-p)
        (py-isort-region)
      (message "Select a region before to call isort")))
  :bind (:map elpy-mode-map
              ("C-c C-i" . my/sort-imports)))

(defun my/run-django-command ()
  "Run a django command."
  (interactive)
  (let* ((python-bin (concat (getenv "VIRTUAL_ENV") "/bin/python"))
         (manage-py-file (concat (projectile-project-root) "manage.py"))
         (default-directory (projectile-project-root))
         (raw-help (shell-command-to-string (concat python-bin " " manage-py-file " help")))
         (splited-lines (split-string raw-help "\n"))
         (options (seq-filter '(lambda (line) (cl-search "    " line)) splited-lines))
         (selection (completing-read "Pick django command: " (mapcar 'string-trim options)))
         (command (concat python-bin " " manage-py-file " " selection)))
    (compile command)))

;; (reformatter-define python-black-format
;;   :program "black"
;;   :args '("-")
;;   :group 'python)

;; (defun python-template ()
;;   "Python Template."
;;   (interactive)
;;   (insert "#!/usr/bin/env python\n# -*- coding: utf-8 -*-\n\n"))

;; (add-hook 'python-mode-hook
;;           '(lambda () (when (eq (buffer-size) 0) (python-template))))

(use-package pony-mode
  :after (python)
  :init
  (defun pony-remove-beautify-html ()
    (remove-hook 'before-save-hook 'web-beautify-html-buffer t))
  :config
  (add-hook 'pony-tpl-minor-mode-hook #'pony-remove-beautify-html))

(use-package virtualenvwrapper
  :after (python)
  :commands
  (venv-workon
   venv-deactivate
   venv-initialize-interactive-shells
   venv-initialize-eshell)
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  :config
  (add-hook 'venv-postmkvirtualenv-hook
            (lambda ()
              (shell-command "pip install jedi rope isort epc importmagic yapf pylint autopep8 flake8 virtualenvwrapper"))))

(use-package pippel)

(use-package yapfify
  :diminish yapf-mode
  :init (add-hook 'python-mode-hook 'yapf-mode))

(use-package importmagic
  :disabled
  :diminish 'importmagic-mode
  :init
  (defun before-save-py-importmagic-fix ()
    (when (eq major-mode 'python-mode) (importmagic-fix-imports)))
  (setq importmagic-be-quiet t)
  (add-hook 'before-save-hook #'before-save-py-importmagic-fix))

(use-package isortify
  :ensure t
  :config
  (add-hook 'python-mode-hook 'isort-mode))

(use-package sphinx-doc
  :after (python)
  :init (add-hook 'python-mode-hook #'sphinx-doc-mode))

(use-package pydoc-info)

(use-package company-anaconda
  :if (executable-find "python")
  :after (company)
  :bind (:map python-mode-map
              ("M-." . anaconda-mode-find-definitions)
              ("M-," . anaconda-mode-find-assignments)
              ("M-r" . anaconda-mode-find-references)
              ("M-*" . anaconda-mode-go-back))
  :diminish anaconda-mode
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list
               (make-local-variable 'company-backends)
               '(company-anaconda :with company-yasnippet company-capf))))
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package python
  :if (executable-find "python")
  :bind (:map python-mode-map
              ("C-c ," . python-indent-shift-left)
              ("C-c ." . python-indent-shift-right))
  :interpreter ("python" . python-mode)
  :init
  (defun python-indent-shift-left ()
    (interactive)
    (if (fboundp 'hydra-py/python-indent-shift-left)
        (hydra-py/python-indent-shift-left)
      (python-indent-shift-left)))

  (defun python-indent-shift-right ()
    (interactive)
    (if (fboundp 'hydra-py/python-indent-shift-right)
        (hydra-py/python-indent-shift-right)
      (python-indent-shift-right)))

  (defun def-python-mode ()
    (electric-indent-local-mode -1))

  (setf python-shell-interpreter "python")
  (add-hook 'python-mode-hook #'def-python-mode)
  :config
  (global-set-key [remap python-indent-shift-left] 'python-indent-shift-left)
  (global-set-key [remap python-indent-shift-right] 'python-indent-shift-right))

(provide 'config)
;;; config.el ends here
