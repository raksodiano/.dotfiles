;;; config.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
;; (use-package afternoon-theme
;;   :ensure t
;;   :config
;;   (load-theme 'afternoon t))

(use-package dracula-theme
  :straight t
  :config
  (load-theme 'dracula t)
  (set-face-foreground 'font-lock-variable-name-face "gray"))

;; (treemacs-create-theme "Atom"
;;   :config
;;   (progn
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "repo"
;;                     :height 1.5
;;                     :v-adjust -0.2
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (root))
;;     (treemacs-create-icon
;;      :icon (format "%s %s "
;;                    (all-the-icons-octicon
;;                     "chevron-down"
;;                     :height 0.75
;;                     :v-adjust 0.1
;;                     :face '(:inherit font-lock-doc-face :slant normal))
;;                    (all-the-icons-octicon
;;                     "file-directory"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (dir-open))
;;     (treemacs-create-icon
;;      :icon (format "%s %s "
;;                    (all-the-icons-octicon
;;                     "chevron-right"
;;                     :height 0.75
;;                     :v-adjust 0.1
;;                     :face '(:inherit font-lock-doc-face :slant normal))
;;                    (all-the-icons-octicon
;;                     "file-directory"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (dir-closed))
;;     (treemacs-create-icon
;;      :icon (format "%s %s "
;;                    (all-the-icons-octicon
;;                     "chevron-down"
;;                     :height 0.75
;;                     :v-adjust 0.1
;;                     :face '(:inherit font-lock-doc-face :slant normal))
;;                    (all-the-icons-octicon
;;                     "package"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (tag-open))
;;     (treemacs-create-icon
;;      :icon (format "%s %s "
;;                    (all-the-icons-octicon
;;                     "chevron-right"
;;                     :height 0.75
;;                     :v-adjust 0.1
;;                     :face '(:inherit font-lock-doc-face :slant normal))
;;                    (all-the-icons-octicon
;;                     "package"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (tag-closed))
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "tag"
;;                     :height 0.9
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (tag-leaf))
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "flame"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (error))
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "stop"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (warning))
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "info"
;;                     :height 0.75
;;                     :v-adjust 0.1
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (info))
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "file-media"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
;;                   "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
;;                   "wav" "mp3" "ogg" "midi"))
;;     ;;    (treemacs-create-icon
;;     ;;     :icon (format "%s "
;;     ;;                   (all-the-icons-octicon
;;     ;;                    "file-code"
;;     ;;                    :v-adjust 0
;;     ;;                    :face '(:inherit font-lock-doc-face :slant normal)))
;;     ;;     :extensions ("yml" "yaml" "sh" "zsh" "fish" "c" "h" "cpp" "cxx" "hpp"
;;     ;;                  "tpp" "cc" "hh" "hs" "lhs" "cabal" "py" "pyc" "rs" "el"
;;     ;;                  "elc" "clj" "cljs" "cljc" "ts" "tsx" "vue" "css" "html"
;;     ;;                  "htm" "dart" "java" "kt" "scala" "sbt" "go" "js" "jsx"
;;     ;;                  "hy" "json" "jl" "ex" "exs" "eex" "ml" "mli" "pp" "dockerfile"
;;     ;;                  "vagrantfile" "j2" "jinja2" "tex" "racket" "rkt" "rktl" "rktd"
;;     ;;                  "scrbl" "scribble" "plt" "makefile" "elm" "xml" "xsl" "rb"
;;     ;;                  "scss" "lua" "lisp" "scm" "sql" "toml" "nim" "pl" "pm" "perl"
;;     ;;                  "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"
;;     ;;                  "cask" "r" "re" "rei" "bashrc" "zshrc" "inputrc" "editorconfig"
;;     ;;                  "gitconfig"))
;;     (treemacs-create-icon
;;      :icon (format "%s "
;;                    (all-the-icons-octicon
;;                     "book"
;;                     :v-adjust 0
;;                     :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
;;                   "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
;;                   "azw3" "kf8" "kfx" "lit" "prc" "mobi" "pkg" "opf" "txt"
;;                   "pdb" "ps" "rtf" "pdg" "xml" "tr2" "tr3" "oxps" "xps"))
;;     (treemacs-create-icon
;;      :icon (format "%s " (all-the-icons-octicon
;;                           "file-text"
;;                           :v-adjust 0
;;                           :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions ("md" "markdown" "rst" "log" "txt" ;; "org"
;;                   "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
;;     (treemacs-create-icon
;;      :icon (format "%s " (all-the-icons-octicon
;;                           "file-binary"
;;                           :v-adjust 0
;;                           :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions ("exe" "dll" "obj" "so" "o" "out"))
;;     (treemacs-create-icon
;;      :icon (format "%s " (all-the-icons-octicon
;;                           "file-pdf"
;;                           :v-adjust 0
;;                           :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions ("pdf"))
;;     (treemacs-create-icon
;;      :icon (format "%s " (all-the-icons-octicon
;;                           "file-zip"
;;                           :v-adjust 0
;;                           :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
;;     (treemacs-create-icon
;;      :icon (format "%s " (all-the-icons-octicon
;;                           "file-text"
;;                           :v-adjust 0
;;                           :face '(:inherit font-lock-doc-face :slant normal)))
;;      :extensions (fallback))))

;; (treemacs-load-theme "Atom")

;; (setq extensions '("org" "yml" "yaml" "sh" "zsh" "fish" "c" "h" "cpp" "cxx" "hpp"
;;                    "tpp" "cc" "hh" "hs" "lhs" "cabal" "py" "pyc" "rs" "el" "elc"
;;                    "clj" "cljs" "cljc" "ts" "tsx" "vue" "css" "html" "htm" "dart"
;;                    "java" "kt" "scala" "sbt" "go" "js" "jsx" "hy" "json" "jl" "ex"
;;                    "exs" "eex" "ml" "mli" "pp" "dockerfile" "vagrantfile" "j2"
;;                    "jinja2" "tex" "racket" "rkt" "rktl" "rktd" "scrbl" "scribble"
;;                    "plt" "makefile" "elm" "xml" "xsl" "rb" "scss" "lua" "lisp"
;;                    "scm" "sql" "toml" "nim" "pl" "pm" "perl" "vimrc" "tridactylrc"
;;                    "vimperatorrc" "ideavimrc" "vrapperrc" "cask" "r" "re" "rei"
;;                    "bashrc" "zshrc" "inputrc" "editorconfig" "gitconfig" "gitignore"))

;; (dolist (extension extensions)
;;   (treemacs-define-custom-icon (all-the-icons-icon-for-file (concat "name." extension)) extension))

(provide 'config)
;;; config.el ends here
