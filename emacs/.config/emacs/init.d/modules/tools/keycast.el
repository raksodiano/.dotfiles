;;; keycast.el -*- lexical-binding: t; -*-
;;; keycast.el --- Show pressed keys and commands (for demos)  -*- lexical-binding: t; -*-

;;; Commentary:
;; Toggle with SPC t k. Adapted to doom-modeline: it sets `mode-line-format'
;; to (list "%e" <doom format>), so the keycast item must be inserted
;; after the "%e" placeholder, without cutting the rest of the line.

;;; Code:

(use-package keycast
  :commands (keycast-mode-line-mode)
  :init
  (my-leader
   '(("t k" . my/keycast-mode)))
  :custom
  (keycast-mode-line-insert-after "%e")
  (keycast-mode-line-remove-tail-elements nil)
  ;; Blend the item into the doom modeline.
  (keycast-mode-line-format "%10s%k %c")
  :config
  (define-minor-mode my/keycast-mode
    "Toggle keycast in the mode line, doom-modeline aware."
    :global t
    (keycast-mode-line-mode (if my/keycast-mode 1 -1))))

(provide 'my-keycast)
;;; keycast.el ends here
