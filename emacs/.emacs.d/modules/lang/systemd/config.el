;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Editar archivos de servicios de SystemD

;;; Code:
(use-package systemd
  :config
  (add-hook 'systemd-mode-hook #'yas-minor-mode-on))

(provide 'config)
;;; config.el ends here
