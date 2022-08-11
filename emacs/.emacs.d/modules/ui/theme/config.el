;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; (use-package afternoon-theme
;;   :ensure t
;;   :config
;;   (load-theme 'afternoon t))

(use-package kaolin-themes
  :init
  (setq kaolin-themes-bold t
        ;; kaolin-themes-italic t
        kaolin-themes-italic-comments t
        kaolin-themes-underline t
        ;; kaolin-themes-hl-line-colored t
        ;; kaolin-themes-distinct-fringe t
        ;; kaolin-themes-distinct-company-scrollbar t
        kaolin-themes-git-gutter-solid t
        kaolin-ocean-alt-bg t)
  :config
  (load-theme 'kaolin-ocean t)
  ;; (load-theme 'kaolin-aurora t)
  ;; (load-theme 'kaolin-valley-dark t)
  (kaolin-treemacs-theme))

(provide 'config)
;;; config.el ends here
