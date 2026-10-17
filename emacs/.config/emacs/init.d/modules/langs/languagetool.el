;;; languagetool.el -*- lexical-binding: t; -*-
;;; languagetool.el --- Grammar and style checking with LanguageTool  -*- lexical-binding: t; -*-

;;; Commentary:
;; Complements the hunspell setup in core/editing.el: hunspell checks
;; spelling, LanguageTool grammar, style and punctuation. Runs against
;; the public API (https://api.languagetool.org/v2), so it works without
;; Java. To switch to a private local server (no limits, privacy):
;;   1. pacman -S jre-openjdk (CachyOS/Arch ships Java in the repos)
;;   2. Download the LanguageTool zip from https://languagetool.org/download/
;;      and extract it where you like (e.g. ~/.local/share/languagetool)
;;   3. Set flymake-languagetool-server-jar below to the languagetool-server.jar
;;      path and drop the `flymake-languagetool-url' line.

;;; Code:

(use-package flymake-languagetool
  ;; text-mode covers org and markdown (both derive from it); AUCTeX
  ;; needs its own `LaTeX-mode' hook.
  :hook ((text-mode . flymake-languagetool-load)
         (LaTeX-mode . flymake-languagetool-load))
  :custom
  ;; Remote public API by default: free, no Java required.
  (flymake-languagetool-server-jar nil)
  (flymake-languagetool-url "https://api.languagetool.org")
  ;; Spelling stays in hunspell (SPC l c toggles it); LanguageTool only
  ;; fills in what hunspell cannot see: grammar, style, false friends.
  (flymake-languagetool-check-spelling nil)
  (flymake-languagetool-use-categories t)
  ;; Must match the default hunspell dictionary so both checkers agree
  ;; on the language (my/ispell-toggle-dictionary keeps it in sync).
  (flymake-languagetool-language "en-US")
  :config
  ;; The whitespace rule is noisy in org buffers.
  (push "WHITESPACE_RULE" flymake-languagetool-disabled-rules)
  ;; Apply suggestions from the dashboard of diagnostics.
  (my-leader
   '(("c l" . flymake-languagetool-correct-dwim))))

(provide 'my-languagetool)
;;; languagetool.el ends here
