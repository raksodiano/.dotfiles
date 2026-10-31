;;; hugo.el -*- lexical-binding: t; -*-
;;; hugo.el --- Org Hugo export (split out)

;;; Code:

(with-eval-after-load 'ox-hugo
  (let ((blog-dir (expand-file-name "~/Workspace/Blog")))
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

;; Site management: new post, local preview, publish. ox-hugo exports the
;; Org files; easy-hugo runs the Hugo commands. The wrapper explains how
;; to create the site when the post directory does not exist yet.
(defun my/easy-hugo ()
  "Open Easy Hugo, or explain how to create the site."
  (interactive)
  (require 'easy-hugo)
  (let ((posts (expand-file-name easy-hugo-postdir easy-hugo-basedir)))
    (unless (file-directory-p posts)
      (user-error "No Hugo site: run `hugo new site %s'" easy-hugo-basedir))
    (easy-hugo)))

(use-package easy-hugo
  :commands (easy-hugo easy-hugo-newpost easy-hugo-preview easy-hugo-publish)
  :custom
  (easy-hugo-basedir (expand-file-name "~/Workspace/Blog"))
  (easy-hugo-postdir "content/posts")
  (easy-hugo-default-ext ".org")
  (easy-hugo-preview-url "http://localhost:1313/"))

(my-leader
 '(("x h" . my/easy-hugo)))

(provide 'my-org-hugo)
;;; hugo.el ends here
