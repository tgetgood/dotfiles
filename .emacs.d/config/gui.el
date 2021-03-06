;;;;; Customizations specific to window system mode
;;;;; A.K.A.: Turn off all the doodads.

(defun clean-ui ()
	(interactive)
	(progn
		(fringe-mode 0)
		(tool-bar-mode -1)
		(scroll-bar-mode -1)

		(set-face-attribute 'default nil :height 90)

		(set-face-attribute 'mode-line nil :box nil)
		(set-face-attribute 'mode-line-inactive nil :box nil)

		(set-face-italic-p 'italic nil)
		(make-face-unitalic 'font-lock-comment-face)

		(setq initial-frame-alist
					'((width . 200) (height . 80)))

		(setq default-frame-alist
					'((width . 200) (height . 80)))))
