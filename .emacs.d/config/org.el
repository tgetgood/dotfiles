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

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))

(define-key org-mode-map (kbd "M-h") nil)

(add-to-list 'org-drawers "NOTE")
(add-to-list 'org-drawers "FINISHED")

(setq org-hide-leading-stars t)

(setq org-cycle-include-plain-lists 'integrate)
