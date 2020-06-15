;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Ibuffer es un remplazo avanzado para BufferMenu.

;;; Code:
(leaf ibuffer
      :bind ("C-x C-b" . ibuffer)
      :config
      (setq ibuffer-saved-filter-groups
            (quote (("default"
                     ("org" (name . "^.*org$"))

                     ("web" (or (mode . web-mode)
                                (mode . php-mode)
                                (mode . js2-mode)))
                     ("shell" (or (mode . eshell-mode)
                                  (mode . term-mode)
                                  (mode . shell-mode)))
                     ("prog" (or (mode . python-mode)
                                 (mode . ruby-mode)
                                 (mode . nxml-mode)
                                 (mode . c++-mode)))
                     ("emacs" (or
                               (name . "^\\*scratch\\*$")
                               (name . "^\\*Messages\\*$")))
                     ))))
      (add-hook 'ibuffer-mode-hook
                (lambda ()
                  (ibuffer-auto-mode 1)
                  (ibuffer-switch-to-saved-filter-groups "default")))

      ;; don't show these
      ;;(add-to-list 'ibuffer-never-show-predicates "zowie")
      ;; Don't show filter groups if there are no buffers in that group
      (setq ibuffer-show-empty-filter-groups nil)

      ;; Don't ask for confirmation to delete marked buffers
      (setq ibuffer-expert t))

;; Shackle
(leaf shackle
      :straight t
      :init (shackle-mode)
      :config
      (setq shackle-default-size 0.4)

      (setq shackle-rules
            '((compilation-mode :select nil)
              ("*undo-tree*" :size 0.25 :align left)
              ("\\*shell*\\*" :select t :other t)
              ("*Shell Command Output*" :select nil)
              ("\\*Async Shell.*\\*" :regexp t :ignore t)
              (occur-mode :select nil :align t)
              ("*Help*" :select t :inhibit-window-quit t :other t)
              ("*Completions*" :size 0.3  :align t)
              ("*Messages*" :select nil :inhibit-window-quit t :other t)
              ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)
              ("\\*poporg.*\\*" :regexp t :select t :other t)
              ("\\`\\*ivy.*?\\*\\'" :regexp t :size 0.3 :align t)
              ("*Calendar*" :select t :size 0.3 :align below)
              (pdf-view-mode :other t)
              ("*edbi-dbviewer*" :regexp t :select t :size 0.3))))

(provide 'config)
;;; config.el ends here
