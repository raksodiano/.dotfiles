;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Editar archivos de servicios de SystemD

;;; Code:
(leaf systemd
      :straight t
      :config
      (add-hook 'systemd-mode-hook #'yas-minor-mode-on))

(provide 'config)
;;; config.el ends here
