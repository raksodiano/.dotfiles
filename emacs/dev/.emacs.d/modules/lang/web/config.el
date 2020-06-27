;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf web-beautify
      :straight t
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

(leaf emmet-mode
      :straight t
      :config
      (add-hook 'sgml-mode-hook 'emmet-mode)
      (add-hook 'web-mode-hook 'emmet-mode))

(leaf company-web
      :straight t
      :after (web-mode)
      :config
      (add-hook 'web-mode-hook (lambda ()
                                 (add-to-list
                                  (make-local-variable 'company-backends)
                                  '(company-web-html :with company-yasnippet)))))

(leaf yaml-mode
      :straight t
      :leaf-defer 10
      :mode "\\.yml$")

(leaf web-mode
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
