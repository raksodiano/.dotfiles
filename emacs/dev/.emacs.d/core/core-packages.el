;;; core-packages.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:

;; Install leaf using straight
(straight-use-package 'leaf)
(straight-use-package 'leaf-keywords)
(straight-use-package
 '(org-plus-contrib
   :repo "https://code.orgmode.org/bzg/org-mode.git"
   :local-repo "org"
   :files (:defaults "contrib/lisp/*.el")
   :includes (org)))

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

(leaf leaf
      :require t
      :init
      (leaf leaf-keywords
            :emacs> 24.4
            :require t
            ;; :setq (leaf-defaults . '(:straight t))
            :init
            (leaf-keywords-init)))

(leaf async
      :straight t
      :leaf-defer nil
      :setq (async-bytecomp-package-mode . t))

(leaf bind-key
      :straight t)

(leaf validate
      :straight t)

(leaf diminish
  :straight t)

(diminish 'undo-tree-mode)
(diminish 'hs-minor-mode)
(diminish 'auto-revert-mode)

(leaf package-lint
  :straight t)

(leaf hydra
  :straight t)

(leaf speed-type
  :straight t)

(leaf vlf-setup
  :straight vlf
  :init
  (setf vlf-application 'dont-ask))

(provide 'core-packages)
;;; core-packages.el ends here
