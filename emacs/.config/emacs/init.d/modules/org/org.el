;;; org.el -*- lexical-binding: t; -*-
;;; org.el --- Org base (formerly Doom's org.el)  -*- lexical-binding: t; -*-

;;; Code:

(my/create-directories)

(setq org-agenda-files
      (append (when (file-directory-p +my/org-agenda-dir)
                (directory-files-recursively +my/org-agenda-dir "\.org$"))
              (when (file-directory-p +my/org-notes-dir)
                (directory-files-recursively +my/org-notes-dir "\.org$"))))

(use-package org
  :ensure nil
  :custom
  (org-startup-indented t)
  (org-modules '(org-habit))
  (org-directory +my/org-base-dir)
  (org-clock-sound (expand-file-name "sounds/bell.wav" my/assets-dir))
  (org-edit-src-content-indentation 2)
  ;; org-agenda-files assigned in :config to avoid opening missing dirs early.
  :config
  (eval-after-load 'evil
    (evil-define-key 'normal org-mode-map
      (kbd "<M-left>") #'org-do-promote
      (kbd "<M-right>") #'org-do-demote)))

;; Agenda custom commands based on work directory.
(setq org-agenda-files-work
      (directory-files-recursively +my/org-work-dir "\\.org$"))

(setq org-agenda-custom-commands
      '(("w" "Work Agenda"
         ((agenda "" ((org-agenda-files org-agenda-files-work)))
          (todo "" ((org-agenda-files org-agenda-files-work)))))))

(setq-default fill-column 120)
(setq visual-fill-column-width 120
      visual-fill-column-center-text t)

;; Custom todo keywords and faces.
(defface +org-todo-wait '((t (:inherit (bold warning org-todo))))
  "Face for WAIT tasks.")
(defface +org-todo-active '((t (:inherit (bold org-todo))))
  "Face for NEXT tasks.")
(defface +org-todo-onhold '((t (:inherit (italic org-todo))))
  "Face for EVENT tasks.")
(defface +org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo))))
  "Face for PROJ tasks.")
(defface +org-todo-cancel '((t (:inherit (shadow org-done))))
  "Face for CANCELLED tasks.")

(setq org-todo-keywords
      '((sequence
         "TODO(t)" "NEXT(n)" "PROJ(p)" "WAIT(w)" "IDEA(i)" "EVENT(e)"
         "|"
         "DONE(d)" "CANCELLED(c)")))

(setq org-todo-keyfaces
      '(("TODO" . +org-todo-active)
        ("NEXT" . +org-todo-active)
        ("PROJ" . +org-todo-project)
        ("WAIT" . +org-todo-wait)
        ("IDEA" . +org-todo-onhold)
        ("EVENT" . +org-todo-onhold)
        ("DONE" . org-done)
        ("CANCELLED" . +org-todo-cancel)))

(defun my/org-mode-set-language ()
  "Set Hunspell dictionary from #+LANGUAGE in the buffer."
  (when (derived-mode-p 'org-mode)
    (let ((lang (cdr (assoc "LANGUAGE" (org-collect-keywords '("LANGUAGE"))))))
      (when lang
        (setq-local ispell-dictionary (downcase (car lang)))))))

(add-hook 'org-mode-hook #'my/org-mode-set-language)

;; Deft (notes search)
(use-package deft
  :ensure nil
  :custom
  (deft-directory +my/org-base-dir)
  (deft-recursive t)
  (deft-extensions '("org" "md" "txt"))
  (deft-default-extension "org")
  (deft-ignore-file-regexp "\\.#\\|~$"))

;; Enable visual line mode in org buffers.
(add-hook 'org-mode-hook #'visual-line-mode)

(provide 'my-org)
;;; org.el ends here
