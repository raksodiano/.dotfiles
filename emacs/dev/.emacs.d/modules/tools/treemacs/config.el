;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf treemacs
      :straight t
      :init
      (with-eval-after-load 'winum
        (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
      :config
      (progn
        (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
              treemacs-deferred-git-apply-delay      0.5
              ;; treemacs-directory-name-transformer    #'identity
              treemacs-display-in-side-window        t
              treemacs-eldoc-display                 t
              treemacs-file-event-delay              5000
              ;; treemacs-file-extension-regex          treemacs-last-period-regex-value
              treemacs-file-follow-delay             0.2
              ;; treemacs-file-name-transformer         #'identity
              treemacs-follow-after-init             t
              treemacs-git-command-pipe              ""
              treemacs-goto-tag-strategy             'refetch-index
              treemacs-indentation                   2
              treemacs-indentation-string            " "
              treemacs-is-never-other-window         nil
              treemacs-max-git-entries               5000
              treemacs-missing-project-action        'ask
              ;; treemacs-move-forward-on-expand        nil
              treemacs-no-png-images                 nil
              treemacs-no-delete-other-windows       t
              ;; treemacs-project-follow-cleanup        nil
              treemacs-persist-file                  (expand-file-name "treemacs-persist" cache-dir)
              ;; treemacs-position                      'left
              treemacs-recenter-distance             0.1
              treemacs-recenter-after-file-follow    nil
              treemacs-recenter-after-tag-follow     nil
              treemacs-recenter-after-project-jump   'always
              treemacs-recenter-after-project-expand 'on-distance
              treemacs-show-cursor                   nil
              treemacs-show-hidden-files             nil ;; hidden files
              treemacs-silent-filewatch              nil
              treemacs-silent-refresh                nil
              treemacs-sorting                       'alphabetic-asc
              treemacs-space-between-root-nodes      t
              treemacs-tag-follow-cleanup            t
              treemacs-tag-follow-delay              1.5
              ;; treemacs-user-mode-line-format         t
              ;; treemacs-user-header-line-format       t
              treemacs-width                         28)

        ;; The default width and height of the icons is 22 pixels. If you are
        ;; using a Hi-DPI display, uncomment this to double the icon size.
        (treemacs-resize-icons 18)

        (treemacs-follow-mode t)
        (treemacs-filewatch-mode t)
        (treemacs-fringe-indicator-mode t)
        (pcase (cons (not (null (executable-find "git")))
                     (not (null treemacs-python-executable)))
          (`(t . t)
           (treemacs-git-mode 'deferred))
          (`(t . _)
           (treemacs-git-mode 'simple))))
      :bind
      (global-map
       ("M-0"       . treemacs-select-window)
       ("C-x t 1"   . treemacs-delete-other-windows)
       ([f9] . treemacs-project-follow-cleanup)
       ("C-x t B"   . treemacs-bookmark)
       ("C-x t C-t" . treemacs-find-file)
       ("C-x t M-t" . treemacs-find-tag)))

(bind-key [f8] 'treemacs)

(leaf treemacs-projectile
      :after treemacs projectile
      :straight t
      :config
      (setq treemacs-header-function #'treemacs-projectile-create-header))

(leaf treemacs-magit
  :after treemacs magit
  :straight t)

(leaf treemacs-persp
      :after treemacs persp-mode
      :straight t)
;; :config
;; (treemacs-set-scope-type 'Perspectives))

(provide 'config)
;;; config.el ends here
