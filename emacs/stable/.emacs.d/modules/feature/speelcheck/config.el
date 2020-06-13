;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(use-package flycheck-package
  :after (flycheck))

(use-package flyspell
  :diminish ""
  :bind (:map flyspell-mode-map
              ("\M-\t" . nil)
              ([down-mouse-2] . nil)
              ([mouse-2] . nil))
  :init
  (dolist
      (hook '(text-mode-hook message-mode-hook markdown-mode-hook org-mode-hook))
    (add-hook hook 'turn-on-flyspell))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  :config
  (setq flyspell-use-meta-tab nil
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (setq-default ispell-program-name    "hunspell"
                ispell-really-hunspell t
                ispell-check-comments  t
                ispell-extra-args      '("-i" "utf-8")
                ispell-dictionary      "en_US")

  (defun switch-dictionary ()
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "en_US") "es_ES" "en_US")))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s" dic change)))

  (defun turn-on-spell-check ()
    (flyspell-mode 1))

  (use-package flyspell-correct
    :ensure t
    :after flyspell
    :bind (:map flyspell-mode-map
                ("C-c s" . flyspell-correct-word-generic))
    :config
    (setq flyspell-correct-interface 'flyspell-correct-popup))

  (defun flyspell-add-word-to-dict ()
    "Add the word at the current location to the private dictionary without question."
    (interactive)
    ;; use the correct dictionary
    (flyspell-accept-buffer-local-defs)
    (setq opoint (point-marker))
    (let ((cursor-location (point))
          (word (flyspell-get-word nil)))
      (if (consp word)
          (let ((start (car (cdr word)))
                (end (car (cdr (cdr word))))
                (word (car word)))
            ;; The word is incorrect, we have to propose a replacement.
            (flyspell-do-correct 'save nil word cursor-location start end opoint)))
      (ispell-pdict-save t)))

  (define-key flyspell-mode-map [(control ?\')] 'flyspell-add-word-to-dict))

(bind-key "C-c t s" 'flyspell-mode)
(bind-key "C-c t b" 'flyspell-buffers)
(bind-key "M-i" 'switch-dictionary)

(provide 'config)
;;; config.el ends here
