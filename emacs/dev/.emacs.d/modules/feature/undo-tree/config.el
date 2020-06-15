;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(leaf undo-tree
      ;;   http://melpa.milkbox.net/#/undo-tree
      ;; :demand t
      ;; :diminish undo-tree-mode
      :bind (("C-x u" . undo-tree-visualizer))
      ;; (:map
      ;;  undo-tree-visualizer-mode-map ("RET" . undo-tree-visualizer-quit)))
      :init
      (defadvice undo-tree-make-history-save-file-name
          (after undo-tree activate)
        (setq ad-return-value (concat ad-return-value ".7z")))

      (defadvice undo-tree-visualize (around undo-tree-split-side-by-side activate)
        "Divide la ventana de lado a lado al visualizar undo-tree-visualize"
        (let ((split-height-threshold nil)
              (split-width-threshold 0))
          ad-do-it))

      (setf undo-tree-visualizer-timestamps t)
      (setf undo-tree-visualizer-diff t)
      (setf undo-tree-auto-save-history nil) ;; no salva el historial de cambios

      :config
      (defalias 'redo 'undo-tree-redo)
      (global-undo-tree-mode 1))

(provide 'config)
;;; config.el ends here
