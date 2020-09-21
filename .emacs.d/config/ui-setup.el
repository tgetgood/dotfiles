;;;;; Customizations specific to window system mode
;;;;; A.K.A.: Turn off all the doodads.

(use-package cyberpunk-theme
  :demand t
  :config
  (defun cyberpunk-it ()
    (load-theme 'cyberpunk t)
    (set-face-attribute 'mode-line nil :box nil)
    (set-face-attribute 'mode-line-inactive nil :box nil)))

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
       (unsplittable . t)))

(setq initial-frame-alist my-frame-alist)
 
(setq default-frame-alist my-frame-alist)

(fringe-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-face-attribute 'default nil
                    :family "Hack"
                    :height 90)

;; Seems obsolete
;; (set-face-italic-p 'italic nil)

(make-face-unitalic 'font-lock-comment-face)

(defun light ()
  "Sets light background theme."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (load-theme 'leuven))

(defun dark ()
  "Sets dark background theme."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme))
  (cyberpunk-it))

(dark)
