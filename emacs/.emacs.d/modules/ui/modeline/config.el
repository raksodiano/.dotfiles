;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; (use-package telephone-line
;;   :init
;;   (setq telephone-line-primary-left-separator 'telephone-line-tan-left
;;         telephone-line-secondary-left-separator 'telephone-line-tan-hollow-left
;;         telephone-line-primary-right-separator 'telephone-line-tan-right
;;         telephone-line-secondary-right-separator 'telephone-line-tan-hollow-right)
;;   (setf telephone-line-height 24
;;         telephone-line-evil-use-short-tag t)
;;   :config
;;   (telephone-line-defsegment rakso-line-buffer-segment ()
;;     (telephone-line-raw mode-line-buffer-identification))

;;   (telephone-line-defsegment rakso-vc-segment ()
;;     (if (rakso-vc-info)
;;         (telephone-line-raw (concat (rakso-buffer-vc-modified) "  " (rakso-vc-info)))
;;       (telephone-line-raw (rakso-buffer-vc-modified))))

;;   (telephone-line-defsegment rakso-flycheck-status-segment ()
;;     (telephone-line-raw (rakso-flycheck-status)))

;;   (telephone-line-defsegment rakso-date-time-segment ()
;;     (shell-command-to-string "echo -n $(date '+%a %d %b %k:%M%p')"))

;;   (setf telephone-line-lhs
;;         '((accent . (rakso-line-buffer-segment))
;;           (nil .  (rakso-vc-segment rakso-flycheck-status-segment))))

;;   (setf telephone-line-rhs '((evil . (telephone-line-misc-info-segment telephone-line-major-mode-segment))
;;                              (accent .(rakso-date-time-segment))
;;                              (evil . (telephone-line-airline-position-segment))))

;;   (telephone-line-mode 1))

(use-package doom-modeline
  :straight t
  :defer t
  :init
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-number-limit 99)
  (setq doom-modeline-persp-name t)
  :custom (doom-modeline-modal-icon nil)
  :hook (after-init . doom-modeline-mode)
  :config
  (add-hook 'doom-modeline-mode-hook '(lambda ()
                                        (display-battery-mode))))

(provide 'config)
;;; config.el ends here
