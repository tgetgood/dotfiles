;;;;; Customizations specific to window system mode
;;;;; A.K.A.: Turn off all the doodads.

(setq initial-frame-alist
      '((width . 100) (height . 80)))

(setq default-frame-alist
      '((width . 100) (height . 80)))

(defun clean-ui ()
	(interactive)
	(progn
		(fringe-mode 0)
		(tool-bar-mode -1)
		(scroll-bar-mode -1)

		(set-face-attribute 'default nil
                        :family "Hack"
                        :height 90)

		(set-face-attribute 'mode-line nil :box nil)
		(set-face-attribute 'mode-line-inactive nil :box nil)

    ;; Seems obsolete
		;; (set-face-italic-p 'italic nil)

		(make-face-unitalic 'font-lock-comment-face)))
