;;;;;
;; Truly global editor config
;;;;;

(menu-bar-mode -1)
(global-linum-mode 1)
(column-number-mode)
(show-paren-mode)
(setq-default tab-width 2)
(global-auto-revert-mode 1)

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

(ido-mode)

(require 'undo-tree)
(global-undo-tree-mode t)

;;;;;
;; Misc special funcs
;;;;;

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "M-;") 'toggle-comment-on-line)

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
