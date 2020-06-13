;;; core-constants.el --- .Emacs Configuration -*- lexical-binding: t -*-
;;; Commentary:
;;

;;; Code:
(defvar org-dir  "~/org/"
  "Archivos de tareas varias ORG.")

(defvar org-imagenes-dir (concat org-dir "imagenes/")
  "Archivos de imagenes descargadas por ORG.")

(defvar workspace-dir "~/Workspace/"
  "Carpeta de trabajos y practicas.")

(defvar latex-dir  (concat workspace-dir "LaTeX/")
  "Archivos LaTeX.")

(defvar backup-dir (concat user-emacs-directory ".backup/")
  "Aquí se guardan los backups generados.")

(defvar cache-dir (concat user-emacs-directory ".cache/")
  "Archivos volatiles.")

(defvar etc-dir (concat user-emacs-directory ".etc/")
  "Archivos de uso continuo.")

(defvar local-dir (concat user-emacs-directory ".local/")
  "Carpeta local para almacenar archivos que no se tocaran.")

(defvar backup-tramp-dir (concat backup-dir "tramp/")
  "Archivos tramp.")

(defvar cache-server-dir (concat cache-dir "server/")
  "Almacen de server's.")

(defvar tramp-dir (concat cache-dir "tramp/")
  "Tramp.")

(defvar cache-tramp-dir (concat cache-dir "tramp-auto-save/")
  "Sesiones ssh guardadas.")

(defvar cache-url-dir (concat cache-dir "url/")
  "URL's volatiles.")

(defvar shared-game-score-dir (concat etc-dir "shared-game-score/")
  "Score de los juegos.")

(defvar url-dir (concat etc-dir "url/")
  "Almacen de URL's importantes.")

(defvar bookmark-dir (concat local-dir "bookmark/")
  "Bookmark Save.")

(defvar packages-dir (concat local-dir "packages/")
  "Paquetes instalados.")

(defvar projectile-dir (concat local-dir "projectile/")
  "Projectile Save.")

(defvar straight-dir (concat local-dir "straight/")
  "Straight Save.")

(dolist
    (dir
     (list org-dir
           org-imagenes-dir
           workspace-dir
           latex-dir
           ;; w-org-dir
           backup-dir
           cache-dir
           etc-dir
           local-dir
           backup-tramp-dir
           cache-server-dir
           tramp-dir
           cache-tramp-dir
           cache-url-dir
           shared-game-score-dir
           url-dir
           bookmark-dir
           packages-dir
           projectile-dir
           straight-dir))

  (unless (file-directory-p dir)
    (make-directory dir t)))

(setq straight-install-dir straight-dir)

(provide 'core-constants)
;;; core-constants.el ends here
