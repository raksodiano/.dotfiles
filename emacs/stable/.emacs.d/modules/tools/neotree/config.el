;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package shrink-path
  :ensure t)

(use-package neotree
  :ensure t
  :straight (neotree
             :type git
             :host github
             :repo "jaypei/emacs-neotree"
             :branch "dev")
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neotree-hidwindow-exists-p)
  :custom
  (neo-fit-to-contents t)
  (neo-theme 'icons)
  (neo-autorefresh nil)
  (neo-vc-integration '(face))
  :bind (([f2] . 'my/neotree-toggle)
         :map neotree-mode-map
         ("c" . neotree-change-root)
         ("C" . neotree-create-node)
         ("d" . neotree-delete-node)
         ("r" . 'neotree-rename-node))
  :config
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh t
        neo-smart-open t
        neo-mode-line-type 'none
        neo-window-width 28
        neo-show-updir-line nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise)

  (when (bound-and-true-p winner-mode)
    (push neo-buffer-name winner-boring-buffers))

  (defun shrink-root-entry (node)
    "shrink-print pwd in neotree"
    (insert (propertize (concat (shrink-path-dirs node) "\n") 'face `(:inherit (,neo-root-dir-face)))))

  (advice-add #'neo-buffer--insert-root-entry :override #'shrink-root-entry))

(defun neotree-project-dir-toggle ()
  "Open NeoTree using the project root, using find-file-in-project or the current buffer directory."
  (interactive)
  (let ((project-dir
         (ignore-errors
             ;;; Pick one: projectile or find-file-in-project
           (projectile-project-root)))
        (file-name (buffer-file-name))
        (neo-smart-open t))
    (if (and (fboundp 'neo-global--window-exists-p)
             (neo-global--window-exists-p))
        (neotree-hide)
      (progn
        (neotree-show)
        (if project-dir
            (neotree-dir project-dir))
        (if file-name
            (neotree-find file-name))))))

(defun my/neotree-toggle ()
  "Custom function with some tweaks to be aplied when neotree opens."
  (interactive)
  (if (and (projectile-project-p) (not (neo-global--window-exists-p)))
      (my/neotree-open-projectile)
    (neotree-toggle)))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key [f2] 'neotree-project-dir)

(provide 'config)
;;; config.el ends here
