;;;;; Customizations specific to window system mode
;;;;; A.K.A.: Turn off all the doodads.

(fringe-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; I'm the goddamned Batman!

(set-default-font "DejaVu Sans Mono-9")

(set-face-attribute 'default nil :font "DejaVu Sans Mono-9" :height 90)
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)

(set-face-italic-p 'italic nil)
(make-face-unitalic 'font-lock-comment-face)

(toggle-frame-fullscreen)