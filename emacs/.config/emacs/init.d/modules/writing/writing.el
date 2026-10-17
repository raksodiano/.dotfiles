;;; writing.el -*- lexical-binding: t; -*-
;;; writing.el --- Distraction-free prose writing suite  -*- lexical-binding: t; -*-

;;; Commentary:
;; Toolkit around your writing sessions in org/text buffers:
;;   SPC x w  writeroom (fullscreen, centered, no chrome)          (proceder 2)
;;   SPC x o  olivetti centered text at any time                   (proceder 1)
;;   SPC _ t  powerthesaurus on the word at point                  (proceder 5)
;;            + wc-mode word/char counter on every prose buffer    (proceder 3)
;;            + writegood passive-voice / weasel-word highlights   (proceder 4)
;; olivetti lives in org/writer.el (it powers o W f / o W s); reusing
;; it here means writers get one consistent body width everywhere.
;; Grammar/punctuation errors come from flymake-languagetool and
;; spelling from hunspell; none of that is duplicated here.

;;; Code:

;; writeroom hides the modeline/tabs and maximizes the window; olivetti
;; toggles together with it so entering and leaving writing mode keeps
;; the same centered columns.
(use-package writeroom-mode
  :commands (writeroom-mode)
  :custom
  (writeroom-width 100))
(add-hook 'writeroom-mode-hook #'olivetti-mode)

;; Available from org/writer.el; SPC x o toggles it standalone too.
(use-package olivetti
  :commands (olivetti-mode))

(use-package wc-mode
  :commands (wc-mode)
  :custom
  (wc-modeline-format "[%Ww %Cc]"))
(add-hook 'text-mode-hook #'wc-mode)

(use-package writegood-mode
  :commands (writegood-mode))
(add-hook 'text-mode-hook #'writegood-mode)

(use-package powerthesaurus
  :commands (powerthesaurus-lookup-word-dwim
             powerthesaurus-lookup-word-dwim-selected))

(my-leader
 '(("x w" . writeroom-mode)
   ("x o" . olivetti-mode)
   ("_ t" . powerthesaurus-lookup-word-dwim)))

(provide 'my-writing)
;;; writing.el ends here
