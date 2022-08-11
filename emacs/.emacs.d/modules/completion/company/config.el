;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Company es un famework de Emacs para el completado de texto.

;;; CODE:
(use-package company
  :straight t
  :init
  (setq company-backends '((company-files
                            company-keywords
                            company-capf
                            company-yasnippet)
                           (company-abbrev company-dabbrev)))

  (setq company-auto-complete nil
        company-echo-delay 0
        company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-tooltip-limit 20
        company-transformers '(company-sort-by-occurrence)))

(use-package company-statistics
  :after (company)
  :init
  (setf company-statistics-file (concat cache-dir "company-statistics-cache.el"))
  (add-hook 'after-init-hook 'company-statistics-mode))

(use-package company-quickhelp
  :straight t
  :after company
  :config (company-quickhelp-mode 1))

(defun company-mode/backend-with-yas (backend)
  "Coloque comentario aquí :V ."
  (if (or (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(add-hook 'company-mode-hook
          (lambda ()
            (setq company-backends
                  (mapcar 'company-mode/backend-with-yas company-backends))) t)

(add-hook 'after-init-hook 'global-company-mode)

(provide 'config)
;;; config.el ends here
