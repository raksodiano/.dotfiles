;;; pass.el -*- lexical-binding: t; -*-
;;; pass.el --- Password-store (split out)

;;; Code:

(use-package password-store
  :commands password-store-get password-store-insert)

(use-package pass
  :commands pass
  :after password-store)

(my-leader
 '(("o P" . pass)))

(provide 'my-pass)
;;; pass.el ends here
