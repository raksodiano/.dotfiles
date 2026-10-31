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

;; Proportional text for prose and monospace for code. `text-mode-hook'
;; covers Org and Markdown too, since both derive from `text-mode'. Keep
;; the cursor alone: Meow owns it in every state.
(use-package mixed-pitch
  :commands (mixed-pitch-mode)
  :hook (text-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor nil))

;; Jinx is the modern Enchant-based speller. It is opt-in because Flyspell
;; is already enabled; turning it on disables Flyspell in that buffer so
;; the two do not highlight the same words.
(use-package jinx
  :commands (jinx-mode jinx-correct)
  :custom
  (jinx-languages "en_US es"))

(defun my/jinx-toggle ()
  "Toggle Jinx in the current buffer, disabling Flyspell first."
  (interactive)
  (if (bound-and-true-p jinx-mode)
      (jinx-mode -1)
    (when (bound-and-true-p flyspell-mode)
      (flyspell-mode -1))
    (jinx-mode 1)))

;; Edit browser textareas (GhostText extension) in the current frame, so
;; the buffer inherits the Meow state list.
(use-package edit-server
  :custom
  (edit-server-new-frame nil)
  (edit-server-default-major-mode 'text-mode)
  :config
  (unless noninteractive
    (edit-server-start)))

(my-leader
 '(("x w" . writeroom-mode)
   ("x o" . olivetti-mode)
   ("x m" . mixed-pitch-mode)
   ("x j" . my/jinx-toggle)
   ("c j" . jinx-correct)
   ("_ t" . powerthesaurus-lookup-word-dwim)))

(provide 'my-writing)
;;; writing.el ends here
