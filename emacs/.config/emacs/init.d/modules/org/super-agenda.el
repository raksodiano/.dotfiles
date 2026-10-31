;;; super-agenda.el -*- lexical-binding: t; -*-
;;; super-agenda.el --- Grouped agenda views  -*- lexical-binding: t; -*-

;;; Commentary:
;; org-super-agenda groups the default agenda view using the todo
;; keywords defined in org/org.el (TODO NEXT PROJ WAIT IDEA EVENT |
;; DONE CANCELLED), so the daily agenda stops being a flat wall of
;; tasks. The agenda runs in Meow motion state, so its own motions keep
;; working.

;;; Code:

(use-package org-super-agenda
  :after org-agenda
  :custom
  (org-super-agenda-groups
   '((:name "En curso"
            :todo "NEXT")
     (:name "Vencidas"
            :and (:deadline past :not (:todo "DONE")))
     (:name "Para hoy"
            :and (:scheduled today :not (:todo "DONE")))
     (:name "Bloqueadas"
            :todo "WAIT")
     (:name "Proyectos"
            :todo "PROJ")
     (:name "Ideas"
            :todo "IDEA")
     (:name "Eventos"
            :todo "EVENT")
     (:name "Pendientes"
            :todo "TODO")
     (:discard (:anything t))))
  :config
  (org-super-agenda-mode +1))

(provide 'my-org-super-agenda)
;;; super-agenda.el ends here
