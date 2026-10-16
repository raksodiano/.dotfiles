;;; slides.el -*- lexical-binding: t; -*-
;;; slides.el --- Live Org presentations with org-tree-slide

;;; Code:

(my/create-directories)

(use-package org-tree-slide
  :commands (org-tree-slide-mode)
  :custom
  ;; Emphasize the current slide heading; no slide-in animation.
  (org-tree-slide-heading-emphasis t)
  (org-tree-slide-slide-in-effect nil)
  :config
  (defvar my/slides-saved-mode-line nil
    "Mode line saved while presenting.")

  (defun my/slides-start ()
    "Presentation profile: single window, large text, no mode line."
    (delete-other-windows)
    (setq-local my/slides-saved-mode-line mode-line-format)
    (setq-local mode-line-format nil)
    (text-scale-set 2)
    (display-line-numbers-mode -1)
    (force-mode-line-update))

  (defun my/slides-stop ()
    "Restore the editing profile after presenting."
    (when (local-variable-p 'my/slides-saved-mode-line)
      (setq-local mode-line-format my/slides-saved-mode-line)
      (kill-local-variable 'my/slides-saved-mode-line))
    (text-scale-set 0)
    (force-mode-line-update))

  (add-hook 'org-tree-slide-play-hook #'my/slides-start)
  (add-hook 'org-tree-slide-stop-hook #'my/slides-stop)

  ;; Vim-style deck motion under Evil. The minor mode itself toggles,
  ;; so q quits by turning it off.
  (with-eval-after-load 'evil
    (when (boundp 'org-tree-slide-mode-map)
      (evil-define-key 'normal org-tree-slide-mode-map
        "n" #'org-tree-slide-move-next-tree
        "p" #'org-tree-slide-move-previous-tree
        "q" #'org-tree-slide-mode))))

(defun my/slides-toggle ()
  "Start or stop presenting the current Org buffer as slides."
  (interactive)
  (org-tree-slide-mode (if (bound-and-true-p org-tree-slide-mode) -1 1)))

;; SPC m s presents; SPC m m stays music, SPC m p/v/e stay markdown.
(my-leader
 '(("m s" . my/slides-toggle)))

(provide 'my-org-slides)
;;; slides.el ends here
