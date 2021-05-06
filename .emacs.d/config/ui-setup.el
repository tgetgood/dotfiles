;;;;; Customizations specific to window system mode
;;;;; A.K.A.: Turn off all the doodads.

(unless (package-installed-p 'cyberpunk-theme)
  (package-install 'cyberpunk-theme))

(defun cyberpunk-it ()
  "Load cyberpunk theme and disable excessive sci-fi"
  (load-theme 'cyberpunk t)
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

(defvar my-frame-alist
     '((width . 100)
       (height . 80)
       (vertical-scroll-bars . nil)
       (horizontal-scroll-bars . nil)
       (left-fringe . 0)
       (right-fringe . 0)
       (menu-bar-lines . 0)
       (tool-bar-lines . 0)))

;; TODO: unsplittable is great 90% of the time, but when there' only one window
;; in a frame and I start using magit it gets ugly. Can I exempt certain
;; processes from unspilttable? Related, but not a solution:
;; https://emacs.stackexchange.com/questions/327/how-can-i-block-a-frame-from-being-split

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
