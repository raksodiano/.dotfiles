;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package web-beautify
  :after (web-mode)
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-js-buffer t t)))
  (add-hook 'html-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-html-buffer t t)))
  (add-hook 'css-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'web-beautify-css-buffer t t))))

(use-package emmet-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package company-web
  :after (web-mode)
  :config
  (add-hook 'web-mode-hook (lambda ()
                             (add-to-list
                              (make-local-variable 'company-backends)
                              '(company-web-html :with company-yasnippet)))))

(use-package yaml-mode
  :defer 10
  :mode "\\.yml$")

(use-package web-mode
  :straight t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   ("\\.ctp\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.twig\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(provide 'config)
;;; config.el ends here
