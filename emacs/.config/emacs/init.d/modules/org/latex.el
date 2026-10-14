;;; latex.el -*- lexical-binding: t; -*-
;;; latex.el --- Org LaTeX (split out)

;;; Code:

(with-eval-after-load 'ox-latex
  (let ((class-dir (expand-file-name "latex-classes/" my/assets-dir)))
    (when (file-directory-p class-dir)
      (dolist (main-cls (directory-files-recursively class-dir "main\\.cls" t))
        (let ((class-name (file-name-base (file-name-directory main-cls))))
          (with-temp-buffer
            (insert-file-contents main-cls)
            (add-to-list 'org-latex-classes
                         (list class-name (buffer-string)
                               (cons "\\section{%s}" "\\section*{%s}")
                               (cons "\\subsection{%s}" "\\subsection*{%s}")
                               (cons "\\subsubsection{%s}" "\\subsubsection*{%s}")
                               (cons "\\paragraph{%s}" "\\paragraph*{%s}")
                               (cons "\\subparagraph{%s}" "\\subparagraph*{%s}"))
                         t))))
      (setq org-latex-compiler "xelatex")
      (let ((texinputs (concat class-dir ":")))
        (setq org-latex-pdf-process
              (list (concat "TEXINPUTS=" texinputs " xelatex -interaction nonstopmode -output-directory %o %f")
                    (concat "TEXINPUTS=" texinputs " xelatex -interaction nonstopmode -output-directory %o %f")))))))

(provide 'my-org-latex)
;;; latex.el ends here
