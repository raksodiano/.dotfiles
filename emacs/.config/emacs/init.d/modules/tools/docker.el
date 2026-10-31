;;; docker.el -*- lexical-binding: t; -*-
;;; docker.el --- Docker containers, images and volumes from Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient menus over the docker CLI (Silex/docker). `docker' uses the
;; `docker' executable; `docker-compose' is optional (the package can
;; also drive `docker compose' through it when present).

;;; Code:

(use-package docker
  :commands (docker)
  :init
  ;; String-keyed leader bindings work before the package loads: they
  ;; only reference the command symbol.
  (my-leader
   '(("d d" . docker))))

(provide 'my-docker)
;;; docker.el ends here
