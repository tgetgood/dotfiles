;;;;;
;; Truly global editor config
;;;;;

(menu-bar-mode -1)
(column-number-mode)
(show-paren-mode)
(setq-default tab-width 2)
(global-auto-revert-mode 1)

;; Line numbers with a space at the end.
(global-linum-mode 1)

;; Is there really not a simpler way to do this?
(setq linum-format (lambda (line)
										 (propertize
											(format
											 (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
												 (concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;; TODO: create JS setup file.
(setq js-indent-level 2)

;;;;;
;; Easy window navigation
;;;;;
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;;;;;
;; Setup globalish things
;;;;;

(require 'auto-complete)
(global-auto-complete-mode t)

;;;; ido

(ido-mode t)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode 1)

;; Override default buffer view
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(global-set-key (kbd "C-x C-b") 'ibuffer)


(require 'undo-tree)
(global-undo-tree-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;;;;;
;; Misc special funcs
;;;;;

;;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

;; Override M-x
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)

;;;;;
;; Function overrides
;;;;;

(defun ask-before-closing ()
  "Prompt before closing."
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
