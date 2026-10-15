;;; my-buffers.el -*- lexical-binding: t; -*-
;;; my-buffers.el --- Buffers, files and completion  -*- lexical-binding: t; -*-

;;; Commentary:
;; Ported from Doom's :emacs and :completion modules plus the Dired, consult
;; and vlf sections of config.el.
;;
;; NOTE: consult, consult-buffering, project, dired and flyspell are built
;; into Emacs 31, so they are configured with eval-after-load instead of
;; use-package (which would try to install them from an archive). The
;; :buffer-templates API and consult-buffer-save-window-config no longer
;; exist in current consult. The `capefy' package was folded into `cape';
;; the completion-at-point backends are wired directly instead.

;;; Code:

;;; ### Completion

;; Doom's (company +childframe +tng) becomes Corfu plus Cape. Corfu's popup
;; is now built in, so only the variables that still exist are set.
(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  ;; NOTE: corfu-global-matches, corfu-echo-persistent,
  ;; corfu-cycle-on-no-match and corfu-order were removed in recent
  ;; corfu releases; these are the equivalents that still exist.
  (corfu-preview-current 'insert)
  (corfu-quit-no-match nil)
  (corfu-sort-override-function #'ignore)
  (corfu-max-width 120))

;; Cape provides capf backends. There is no `cape-configure' any more, so
;; the backends are added to completion-at-point-functions directly. LSP
;; completion is wired in langs/lsp.el.
(use-package cape
  :init
  (add-hook 'completion-at-point-functions
            (lambda ()
              (add-completion-at-point-functions
               '(cape-abbrev cape-dabbrev cape-history cape-file cape-line)))
            nil t))

;;; ### Buffer management

;; Doom's (ibuffer +icons).
(eval-after-load 'consult
  (setq consult-buffer-preview-position 'other))

;;; ### Files and directories

;; `project' is built into Emacs now, but it no longer exposes a mutable
;; `project-keymap'; bind the switch/delete/current commands directly.
(with-eval-after-load 'project
  ;; Make switching projects open its dired (Doom's behaviour with
  ;; projectile: the project's file tree appears).
  (setq project-switch-commands
        '((project-dired "Dired")
          (project-find-file "Find file")
          (project-find-regexp "Find regexp")
          (project-find-dir "Find directory")
          (project-vc-dir "VC-Dir")
          (project-eshell "Eshell"))))
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p k") #'project-delete-project)
(global-set-key (kbd "C-c p d") #'project-current)

;; ripgrep as the content search backend, as configured in the Doom module.
(when (executable-find "rg")
  (eval-after-load 'consult
    '(consult-ripgrep-setup)))

;; NOTE: there is no `consult-fd' package on any current archive and
;; consult has no built-in fd commands. dirvish (below) covers finding
;; files interactively.

;;; ### Dired

;; dired is built into Emacs. Doom's (dired +icons +dirvish).
(eval-after-load 'dired
  (lambda ()
    (setq dired-listing-switches "-la")
    (add-hook 'dired-mode-hook #'dired-hide-details-mode)
    ;; Ported from config.el.
    (define-key dired-mode-map (kbd "N") #'dired-create-empty-file)
    (define-key dired-mode-map (kbd "R") #'dired-async-do-rename)
    (define-key dired-mode-map (kbd "C") #'dired-async-do-copy)
    (define-key dired-mode-map (kbd "S") #'dired-async-do-symlink)
    (define-key dired-mode-map (kbd "H") #'dired-async-do-hardlink)
    ;; dired-dwim-target, ported from config.el.
    (setq dired-dwim-target #'dired-dwim-target-recent)))

;; File manager inside Emacs, replacing dired's icons + dirvish module.
(use-package dirvish
  :bind (("C-c C-f" . dirvish-open)
         ("C-c C-d" . dirvish-dwim)
         ("C-c C-p" . dirvish-open-parent))
  :config
  (setq dirvish-default-layout '(0 0 0.4)
        dirvish-layout-recipes '((1 0.11 0.55) (0 0 0.40))
        dirvish-attributes '(vc-state subtree-state nerd-icons file-size)))

(use-package dired-open
  :commands (dired-open dired-open-xdg dired-open-browser dired-open-emacs))

(use-package dired-subtree
  :after dired
  :bind-keymap ("s" . dired-subtree-map))

;;; ### Large files

;; Doom's +large-files: vlf plus so-long, with vlf preserved across so-long.
(use-package vlf
  :commands (vlf)
  :custom
  (vlf-batch-size-remote read-process-output-max)
  :config
  (require 'vlf-setup)
  (add-hook 'vlf-mode-hook #'so-long-mode))

(use-package so-long
  :config
  (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)
  (global-so-long-mode 1))

;;; ### Customisation and bookmarks

;; custom.el is not stowed, so it stays out of the repository.
(setq custom-file (expand-file-name "custom.el" my/config-dir))
(when (file-exists-p custom-file)
  (load custom-file nil t))

  ;; Dirvish (Doom's dired +icons +dirvish).
(use-package dirvish
  :commands (dirvish dirvish-side)
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-default-layout (quote (0 0 0.4))
        dirvish-layout-recipes
        (quote ((1 0.11 0.55) (2 0.22 0.55) (3 0.15 0.55)))
        dirvish-attributes
        (quote (vc-state subtree-state nerd-icons file-size))))


(provide 'my-buffers)
;;; my-buffers.el ends here

