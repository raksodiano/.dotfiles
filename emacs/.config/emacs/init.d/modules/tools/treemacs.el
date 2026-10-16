;;; treemacs.el -*- lexical-binding: t; -*-
;;; treemacs.el --- Treemacs project drawer (added for Evil everywhere)

;;; Code:

(use-package treemacs
  :commands treemacs
  :config
  (setq treemacs-width 35
        treemacs-persist-file
        (expand-file-name "treemacs-persist" my/emacs-cache)
        treemacs-last-error-persist-file
        (expand-file-name "treemacs-persist-at-last-error" my/emacs-cache))
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (add-hook 'treemacs-mode-hook #'(lambda () (display-line-numbers-mode -1))))

(use-package treemacs-evil
  :after (treemacs evil)
  :config
  ;; The kj escape chord must never fire in the tree: ESC runs
  ;; keyboard-escape-quit, which deletes dedicated side windows, so
  ;; mashing j/k closed treemacs "by itself".
  (add-to-list 'evil-escape-excluded-states 'treemacs)
  ;; treemacs-evil leaves root navigation unbound, so going above the
  ;; project root is impossible. Bind dired-style keys for it.
  (evil-define-key 'treemacs treemacs-mode-map
    "-" #'treemacs-root-up
    "u" #'treemacs-root-up
    "+" #'treemacs-root-down))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :config
  (treemacs-projectile-mode +1))

(use-package treemacs-all-the-icons
  :after treemacs
  :config
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode 1))

;; Diagnostic: log every treemacs-quit with its caller chain, so a
;; window that "closes by itself" leaves evidence in *Messages*.
(defun my/treemacs-quit-logger (&rest _)
  "Log the callers of `treemacs-quit' into *Messages*."
  (let ((frames (cdr (backtrace-frames))))
    (message "my/treemacs-quit from: %s"
             (mapconcat (lambda (f) (format "%s" (nth 1 f)))
                        (seq-take frames 8) " <- "))))
(advice-add 'treemacs-quit :before #'my/treemacs-quit-logger)

;; Bind SPC o p to open/select the Treemacs tree for the current project.
(my-leader
 '(("o p" . treemacs)))

(provide 'my-treemacs)
;;; treemacs.el ends here
