;;; core-packages.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

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

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-enable-at-startup nil
      package--init-file-ensured t)

(require 'bind-key)

(use-package validate
  :straight t)

(use-package diminish
  :straight t)

(diminish 'undo-tree-mode)
(diminish 'hs-minor-mode)
(diminish 'auto-revert-mode)

(use-package package-lint
  :straight t)

(use-package hydra
  :straight t)

(use-package speed-type
  :straight t)

(use-package vlf-setup
  :straight vlf
  :init
  (setf vlf-application 'dont-ask))

(provide 'core-packages)
;;; core-packages.el ends here
