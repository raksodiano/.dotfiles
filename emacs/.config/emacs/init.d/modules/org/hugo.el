;;; hugo.el -*- lexical-binding: t; -*-
;;; hugo.el --- Org Hugo export (split out)

;;; Code:

(with-eval-after-load 'ox-hugo
  (let ((blog-dir (expand-file-name "~/Workspace/blog")))
    (setq org-hugo-base-dir blog-dir
          org-hugo-content-directory "content-org"
          org-hugo-section "posts"
          org-hugo-preserve-filing 'force
          org-hugo-auto-set-lastmod t
          org-hugo-export-with-toc nil
          org-hugo-allow-spaces-in-tags t
          org-hugo-paired-shortcodes "note,warning,tip,details"
          org-hugo-taxonomy-tags "tags"
          org-hugo-taxonomy-categories "categories"
          org-hugo-static-file-extensions '("png" "jpg" "jpeg" "gif" "svg" "pdf" "css" "js" "woff" "woff2" "ttf")
          org-hugo-languages '(("es" . "Spanish") ("en" . "English")))))

(use-package ox-hugo
  :defer t
  :after ox)

(provide 'my-org-hugo)
;;; hugo.el ends here
