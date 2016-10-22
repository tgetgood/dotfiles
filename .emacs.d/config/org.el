;; Don't clobber windows
(setq org-agenda-window-setup 'current-window)

;; Agenda
(setq org-agenda-files '("~/org"))

;; Time Tracking
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; TODO config
(setq-default org-highest-priority ?A)
(setq-default org-lowest-priority ?E)
(setq-default org-default-priority ?D)

(setq org-log-done t)
(setq org-log-repeat 'time)
