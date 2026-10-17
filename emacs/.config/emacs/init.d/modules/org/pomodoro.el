;;; pomodoro.el -*- lexical-binding: t; -*-
;;; pomodoro.el --- Org pomodoro timer over org-clock  -*- lexical-binding: t; -*-

;;; Commentary:
;; SPC n p starts a pomodoro on the task at point (needs an inactive
;; clock: org-pomodoro clocks in the current entry). SPC n P kills the
;; current pomodoro session. Sounds use `aplay' (available on this
;; machine) and the notification sounds already bundled with the
;; package; with the org-alert module they also trigger desktop
;; notifications when the org-alert advice counts alerts.

;;; Code:

(use-package org-pomodoro
  :after org
  :custom
  (org-pomodoro-length 25)
  (org-pomodoro-long-break-frequency 4)
  (org-pomodoro-ask-upon-killing t)
  :config
  (my-leader
   '(("n p" . org-pomodoro)
     ("n P" . org-pomodoro-kill))))

(provide 'my-org-pomodoro)
;;; pomodoro.el ends here
