;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; -----------------------------------
;; Editing and utilities
;; -----------------------------------
(package! dotenv-mode)
(package! vlf)
(package! lorem-ipsum)

;; -----------------------------------
;; Dired
;; -----------------------------------
(package! dired-open)
(package! dired-subtree)

;; -----------------------------------
;; Magit
;; -----------------------------------
(package! magit-delta)
;; (package! glab
;;   :recipe (:host github :repo "magit/ghub" :files ("glab.el")))
;; (package! gtea
;;   :recipe (:host github :repo "emacsattic/gtea" :files ("*.el")))
;; (package! gogs
;;   :recipe (:host github :repo "emacsattic/gogs" :files ("*.el")))
;; (package! buck
;;   :recipe (:host github :repo "emacsattic/buck" :files ("*.el")))

;; -----------------------------------
;; Notifications
;; -----------------------------------
(package! alert)
(package! org-alert)

;; -----------------------------------
;; Docker
;; -----------------------------------
(package! docker-compose-mode)

;; -----------------------------------
;; Background process
;; -----------------------------------
(package! detached)

;; -----------------------------------
;; Consult
;; -----------------------------------
(package! consult-company)
(package! consult-projectile)

;; -----------------------------------
;; LaTeX
;; -----------------------------------
(package! latexdiff)

;; -----------------------------------
;; Epub Reader
;; -----------------------------------
(package! nov)
(package! calibredb)

;; -----------------------------------
;; Elfeed
;; -----------------------------------
(package! elfeed-tube)
(package! elfeed-tube-mpv)

;; -----------------------------------
;; Javascript
;; -----------------------------------
(package! prettier-js)

;; -----------------------------------
;; IA
;; -----------------------------------
(package! gptel)
(package! ellama)
(package! llm) ; Dependency for ellama

;; -----------------------------------
;; Markdown
;; -----------------------------------
(package! markdown-soma)
(package! kdl-mode)

;; -----------------------------------
;; Javascript / Typescript
;; -----------------------------------
(package! web-mode)

;; -----------------------------------
;; LSP & Tree-sitter management
;; -----------------------------------
(package! treesit-auto)

;; -----------------------------------
;; Writer
;; -----------------------------------
(package! olivetti)
