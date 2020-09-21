;;;;; Customizations specific to window system mode
;;;;; A.K.A.: Turn off all the doodads.

(use-package cyberpunk-theme
  :defines cyberpunk
  :demand t
  :config (load-theme 'cyberpunk t))

(defvar my-frame-alist
     '((width . 100)
       (height . 80)
       (vertical-scroll-bars . nil)
       (horizontal-scroll-bars . nil)
       (left-fringe . 0)
       (right-fringe . 0)
       (menu-bar-lines . 0)
       (tool-bar-lines . 0)
       ;; REVIEW: This might confuse and annoy me. Alternately, it might be a godsend.
       (unsplittable . t)
       (font-backend . (xft))
       (font . -SRC-Hack-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1)
       ))

(setq initial-frame-alist my-frame-alist)
 

(setq default-frame-alist my-frame-alist)

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

(make-face-unitalic 'font-lock-comment-face)
