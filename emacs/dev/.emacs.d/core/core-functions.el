;;; core-functions.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Functions

;;; Code:
(defun auto-revert-mode ()
  "Recargar todos los buffers abiertos."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(defun empty-buffer? ()
  "Empty Buffer."
  (= (buffer-end 1) (buffer-end -1)))

(defun browse-homepage ()
  "Browse The Notabug page of Emacs."
  (interactive)
  (browse-url rakso-homepage))

(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example
         (expand-file-name "custom-example.el" user-emacs-directory)))
    (unless (file-exists-p custom-file)
      (if (file-exists-p custom-example)
          (copy-file custom-file)
        (error "Unable to find \"%s\"" custom-example)))
    (find-file custom-file)))

(defun rakso-split-window-vertically ()
  "Divide la ventana por la mitad verticalmente y mueve el cursor a la ventana nueva"
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun rakso-split-window-horizontally ()
  "Divide la ventana por la mitad horizontalmente y mueve el cursor a la ventana nueva"
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(global-set-key [remap split-window-below] #'rakso-split-window-vertically)
(global-set-key [remap split-window-right] #'rakso-split-window-horizontally)

;;
;; Carga de todos los archivos dinamicamente
;;

(eval-when-compile
  (defun file-name-is-elisp? (file-name)
    "Say if the extension of a FILE NAME implies Emacs Lisp code.
This is done by checking if the extension is present in
`load-suffixes'."
    (let ((file-suffix (concat "." (downcase (file-name-extension file-name))))
          (emacs-lisp-suffixes (mapcar 'downcase load-suffixes)))
      (if (member file-suffix emacs-lisp-suffixes) t nil)))

  (defun load-directory (directory &optional recursive)
    "Load the Emacs Lisp files in a DIRECTORY.
  Operate recursively in its subdirectories if RECURSIVE is non-nil."
    (let ((visited nil))
      (dolist (name (directory-files directory t))
        (if (not (string-match "/\\.[^/]*$" name))
            (if (file-directory-p name)
                (when recursive (load-directory name t))
              (when (file-name-is-elisp? name)
                (let ((file (file-name-sans-extension name)))
                  (unless (member file visited)
                    (push file visited)
                    (load file))))))))))

(provide 'core-functions)
;;; core-functions.el ends here
