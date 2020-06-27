;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf page-break-lines
      :straight t
      :diminish page-break-lines-mode
      :config (turn-on-page-break-lines-mode))

(leaf dashboard
      :straight t
      :pre-setq `((dashboard-banner-logo-title . ,(concat "GNU Emacs " emacs-version
                                                          " kernel " (car (split-string (shell-command-to-string "uname -r") "-"))
                                                          " x86_64 " (car (split-string (shell-command-to-string "/bin/sh -c '. /etc/os-release && echo $PRETTY_NAME'") "\n"))))
                  (dashboard-startup-banner . 'logo)
                  (dashboard-center-content . t)
                  (dashboard-set-heading-icons . t)
                  (dashboard-set-file-icons . t)
                  (dashboard-items . '((recents  . 10)
                                       (bookmarks . 5)
                                       (projects . 5)))
                  (initial-buffer-choice . '(lambda () (switch-to-buffer "*dashboard*"))))
      :config (dashboard-setup-startup-hook))

;; (leaf dashboard
;;       :straight t
;;       ;; :functions (widget-forward
;;       ;;             winner-undo
;;       ;;             open-custom-file
;;       ;;             persp-load-state-from-file
;;       ;;             persp-get-buffer-or-null
;;       ;;             persp-switch-to-buffer)
;;       :preface
;;       (defvar dashboard-recover-layout-p nil)

;;       (defun open-dashboard ()
;;         "Open the *dashboard* buffer and jump to the first widget."
;;         (interactive)
;;         (if (get-buffer dashboard-buffer-name)
;;             (kill-buffer dashboard-buffer-name))
;;         (dashboard-insert-startupify-lists)
;;         (switch-to-buffer dashboard-buffer-name)
;;         (goto-char (point-min))
;;         (dashboard-goto-recent-files)
;;         (delete-other-windows))

;;       (defun restore-session ()
;;         "Restore last session."
;;         (interactive)
;;         (when (bound-and-true-p persp-mode)
;;           (message "Restoring session...")
;;           (condition-case-unless-debug err
;;               (persp-load-state-from-file)
;;             (error
;;              (message "Error: Unable to restore last session -- %s" err)))
;;           (when (persp-get-buffer-or-null persp-special-last-buffer)
;;             (persp-switch-to-buffer persp-special-last-buffer))))

;;       (defun quit-dashboard ()
;;         "Quit dashboard window."
;;         (interactive)
;;         (quit-window t)
;;         (when (and dashboard-recover-layout-p
;;                    (bound-and-true-p winner-mode))
;;           (winner-undo)
;;           (setq dashboard-recover-layout-p nil)))

;;       (defun dashboard-edit-config ()
;;         "Open custom config file."
;;         (interactive)
;;         (quit-dashboard)
;;         (open-custom-file))

;;       (defun dashboard-goto-recent-files ()
;;         "Go to recent files."
;;         (interactive)
;;         (funcall (local-key-binding "r")))

;;       (defun dashboard-goto-bookmarks ()
;;         "Go to bookmarks."
;;         (interactive)
;;         (funcall (local-key-binding "m")))

;;       (defun dashboard-goto-projects ()
;;         "Go to projects."
;;         (interactive)
;;         (funcall (local-key-binding "p")))

;;       (defun dashboard-goto-agenda ()
;;         "Go to agenda."
;;         (interactive)
;;         (funcall (local-key-binding "a")))

;;       ;; :pre-setq '((dashboard-banner-logo-title (concat "GNU Emacs " emacs-version " kernel "
;;       ;;                                                  (car (split-string (shell-command-to-string "uname -r") "-")) " "
;;       ;;                                                  (car (split-string (shell-command-to-string "uname -m") "-")))
;;       ;;                                          dashboard-page-separator "\n\f\n"))

;;       :bind (dashboard-mode-map
;;              ("H" . browse-homepage)
;;              ("E" . dashboard-edit-config)
;;              ("R" . restore-session)
;;              ("q" . quit-dashboard))
;;       :config
;;       (dashboard-setup-startup-hook)
;;       (setq show-week-agenda-p t
;;             dashboard-items '(rakso/dashboard-menu-sections)
;;             dashboard-items '((recents  . 5)
;;                               (bookmarks . 5)
;;                               (projects . 5)
;;                               (agenda . 5)
;;                               ;; (registers . 5)
;;                               ))
;;       (defun dashboard-insert-buttons (_list-size)
;;         (insert "\n")
;;         (insert (make-string (max 0 (floor (/ (- dashboard-banner-length 76) 2))) ?\ ))
;;         (widget-create 'url-link
;;                        :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
;;                        :help-echo "Abrir Repositorio de Configuración"
;;                        :mouse-face 'highlight
;;                        ;; rakso-homepage
;;                        )
;;         (insert " ")
;;         (widget-create 'push-button
;;                        :help-echo "Restaurar Sesión Previa"
;;                        :action (lambda (&rest _) (restore-session))
;;                        :mouse-face 'highlight
;;                        :button-prefix ""
;;                        :button-suffix ""
;;                        (propertize "Restore Session" 'face 'font-lock-keyword-face))
;;         (insert " ")
;;         (widget-create 'push-button
;;                        :help-echo "Editar Configuración Personal"
;;                        :action (lambda (&rest _) (dashboard-edit-config))
;;                        :mouse-face 'highlight
;;                        :button-prefix ""
;;                        :button-suffix ""
;;                        (propertize "Edit Config" 'face 'font-lock-keyword-face))
;;         (insert " ")
;;         (widget-create 'push-button
;;                        :help-echo "Update Emacs packages"
;;                        :action (lambda (&rest _) (rakso-update))
;;                        :mouse-face 'highlight
;;                        (propertize "Update" 'face 'font-lock-keyword-face))
;;         (insert " ")
;;         (widget-create 'push-button
;;                        :help-echo "Update Emacs packages and restart"
;;                        :action (lambda (&rest _) (rakso-update-and-restart))
;;                        :mouse-face 'highlight
;;                        (propertize "Update and Restart" 'face 'font-lock-keyword-face))
;;         (insert "\n")
;;         (insert "\n")
;;         (insert (make-string (ceiling (max 0 (- dashboard-banner-length 38)) 2) ? )
;;                 (format "[%d packages loaded in %s]" (length package-activated-list) (emacs-init-time))))

;;       (defvar rakso/dashboard-menu-sections
;;         '(("Reload last session"
;;            :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
;;            :when (cond ((require 'persp-mode nil t)
;;                         (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
;;                        ((require 'desktop nil t)
;;                         (file-exists-p (desktop-full-file-name))))
;;            :face (:inherit (doom-dashboard-menu-title bold))
;;            :action doom/quickload-session)
;;           ("Open org-agenda"
;;            :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
;;            :when (fboundp 'org-agenda)
;;            :action org-agenda)
;;           ("Recently opened files"
;;            :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
;;            :action recentf-open-files)
;;           ("Open project"
;;            :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
;;            :action projectile-switch-project)
;;           ("Jump to bookmark"
;;            :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
;;            :action bookmark-jump)
;;           ("Open private configuration"
;;            :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
;;            :when (file-directory-p doom-private-dir)
;;            :action doom/open-private-config)
;;           ("Open documentation"
;;            :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
;;            :action doom/help)))


;;       ;; (add-to-list 'dashboard-item-generators  '(buttons . rakso/dashboard-menu-sections))
;;       (add-to-list 'dashboard-item-generators  '(buttons . dashboard-insert-buttons))
;;       (add-to-list 'dashboard-items '(buttons))

;;       (dashboard-insert-startupify-lists)

;;       (setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))

;;       (with-eval-after-load 'hydra
;;         (defhydra dashboard-hydra (:color red :columns 3)
;;           "Help"
;;           ("<tab>" widget-forward "Next Widget")
;;           ("C-i" widget-forward "Prompt")
;;           ("<backtab>" widget-backward "Previous Widget")
;;           ("RET" widget-button-press "Press Widget" :exit t)
;;           ("g" dashboard-refresh-buffer "Refresh" :exit t)
;;           ("}" dashboard-next-section "Next Section")
;;           ("{" dashboard-previous-section "Previous Section")
;;           ("r" dashboard-goto-recent-files "Recent Files")
;;           ("m" dashboard-goto-bookmarks "Bookmarks")
;;           ("p" dashboard-goto-projects "Projects")
;;           ("a" dashboard-goto-agenda "Agenda")
;;           ("H" browse-homepage "Browse Homepage" :exit t)
;;           ("R" restore-session "Restore Previous Session" :exit t)
;;           ("E" dashboard-edit-config "Open custom file" :exit t)
;;           ("<f2>" open-dashboard "Open Dashboard" :exit t)
;;           ("q" quit-dashboard "Quit Dashboard" :exit t)
;;           ("C-g" nil "quit"))
;;         (bind-keys :map dashboard-mode-map
;;                    ("h" . dashboard-hydra/body)
;;                    ("?" . dashboard-hydra/body))))

(provide 'config)
;;; config.el ends here
