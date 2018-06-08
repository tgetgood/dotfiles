(require 'org)

;; Don't clobber windows
(setq org-agenda-window-setup 'current-window)

;; Agenda
(setq org-agenda-files '("~/org"))

;; Time Tracking
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Just add up hours in clock table. No days, weeks, months, ...
(setq org-time-clocksum-format
			(quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))


;; TODO config
(setq-default org-highest-priority ?A)
(setq-default org-lowest-priority ?E)
(setq-default org-default-priority ?D)

(setq org-log-done t)
(setq org-log-repeat 'time)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))

(define-key org-mode-map (kbd "M-h") nil)

(defvar org-drawers '())

(add-to-list 'org-drawers "NOTE")
(add-to-list 'org-drawers "FINISHED")

(setq org-duration-format '((special . h:mm)))

(setq org-hide-leading-stars t)

(setq org-cycle-include-plain-lists 'integrate)

(defun my-org-clocktable-indent-string (level)
	(if (= level 1)
			""
		(let ((str "^"))
			(while (> level 2)
				(setq level (1- level)
							str (concat str "-")))
			(concat str "> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)
