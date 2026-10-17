;;; alerts.el -*- lexical-binding: t; -*-
;;; alerts.el --- Org alerts (split out of Doom's org.el)

;;; Code:

(unless (boundp (quote my/assets-dir)) (defvar my/assets-dir (expand-file-name "assets" my/config-dir)))

(my/create-directories)

(defun my-sound-file (name)
  (expand-file-name (concat "sounds/" name) my/assets-dir))

(defvar my/sounds-dir (expand-file-name "sounds/" my/assets-dir)
  "Directory containing notification sound files.")

(defvar my/sound-files
  (list (cons 'urgent (my-sound-file "alarm-clock-elapsed.wav"))
        (cons 'high (my-sound-file "window-attention.wav"))
        (cons 'medium (my-sound-file "dialog-warning.wav"))
        (cons 'low (my-sound-file "dialog-information.wav"))
        (cons 'normal (my-sound-file "bell.wav"))
        (cons 'moderate (my-sound-file "message-new-instant.wav")))
  "Sound files mapped by severity level.")

(defun my/play-sound-by-severity (&optional severity)
  "Play notification sound based on SEVERITY."
  (let* ((sev (or severity 'normal))
         (sound-file (cdr (assoc sev my/sound-files)))
         (fallback (cdr (assoc 'normal my/sound-files))))
    (unless (and sound-file (file-exists-p sound-file))
      (setq sound-file fallback))
    (when (and sound-file (file-exists-p sound-file))
      (start-process-shell-command "play-sound" nil
                                   (format "aplay -q %s" sound-file)))))

(defun my/get-org-item-severity ()
  "Get severity from current org item property."
  (let ((sev-str (org-entry-get (point) "NOTIFY_SEVERITY" t)))
    (when sev-str (intern sev-str))))

(defun my/play-notification-sound (severity)
  "Play notification sound with SEVERITY."
  (message "Calling play-notification-sound with severity: %s" severity)
  (my/play-sound-by-severity severity))

(use-package org-alert
  :ensure nil
  :after org
  :custom
  (org-alert-notify-cutoff 1)
  (org-alert-notify-after-event-cutoff 2)
  (org-alert-notification-title "Org Reminder")
  (org-alert-interval 60)
  (alert-default-style 'libnotify)
  :config
  (org-alert-enable))

(defvar my/org-alert-notification-count 0)
(defvar my/alert-called nil)

(advice-add 'alert :around
            (lambda (orig-fun &rest args)
              (setq my/alert-called t)
              (apply orig-fun args)))

(advice-add 'org-alert--dispatch :after
            (lambda (&rest _)
              (when my/alert-called
                (setq my/org-alert-notification-count
                      (1+ my/org-alert-notification-count)
                      my/alert-called nil))))

(advice-add 'org-alert-check :after
            (lambda (&rest _)
              (when (> my/org-alert-notification-count 0)
                (my/play-sound-by-severity 'high))))

;; Notify when an org-to-PDF export finishes (moved from the old
;; media/music.el module, which also held EMMS; exported with
;; `org-latex-export-to-pdf').
(defun my/org-export-pdf-alert (&rest _)
  (alert "Org to PDF export completed"
         :title "Org Export" :severity 'normal))

(with-eval-after-load 'ox-latex
  (advice-add 'org-latex-export-to-pdf :after #'my/org-export-pdf-alert))

(provide 'my-org-alerts)
;;; alerts.el ends here
