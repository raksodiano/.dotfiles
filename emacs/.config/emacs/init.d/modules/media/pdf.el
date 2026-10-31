;;; pdf.el -*- lexical-binding: t; -*-
;;; pdf.el --- PDF support (split out)

;;; Code:

(use-package pdf-tools
  :mode "\\.pdf\\'"
  :config
  (pdf-loader-install)
  (require 'pdf-occur)
  (add-to-list 'revert-without-query ".pdf")
  (add-hook 'pdf-view-mode-hook #'pdf-view-auto-slice-minimap)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
  :bind (:map pdf-view-mode-map
         ("j" . pdf-view-next-line-or-next-page)
         ("k" . pdf-view-previous-line-or-previous-page)
         ("C-=" . pdf-view-enlarge)
         ("C--" . pdf-view-shrink)))

(add-hook 'pdf-view-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (blink-cursor-mode -1)))

(provide 'my-pdf)
;;; pdf.el ends here
