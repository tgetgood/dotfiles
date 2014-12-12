;;;;;
;; Truly global editor config
;;;;;

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(column-number-mode)
(show-paren-mode)
(setq-default tab-width 2)
(global-auto-revert-mode 1)

;;;; Auto-indent

(electric-indent-mode -1)

(global-set-key (kbd "RET") 'newline-and-indent)

;; Line numbers with a space at the end.
(global-linum-mode 1)

;; Is there really not a simpler way to do this?
(setq linum-format
			(lambda (line)
				(propertize
				 (format
					(let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
						(concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;;;;;
;; Stuff that should have separate files
;;;;;

;; TODO: create JS setup file.
(setq js-indent-level 2)

;; TODO: scss file? web file? 
(setq scss-compile-at-save nil)

(global-set-key (kbd "C-c r") 'mc/mark-all-like-this-dwim)

;;;;;
;; Easy window navigation
;;;;;
(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;;;;;
;; Global keywords (...?)
;;;;;

;; Feeding my inline TODO list habit
;; FIXME: This seems an incredibly sub-optimal way to do this...

(add-hook 'buffer-list-update-hook
 (lambda ()
	 (progn
		 (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|HACK\\):"
																		1 font-lock-warning-face t)))
		 (font-lock-fontify-buffer))))

;;;;;
;; Auto completion
;;;;;

(add-hook 'after-init-hook 'global-company-mode)

(defun indent-or-complete ()
	(interactive)
	(if (looking-at "\\_>")
			(company-complete-common)
		(indent-according-to-mode)))

;; Rebind TAB
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(global-set-key (kbd "TAB") 'indent-or-complete)

;;;;;
;; 
;;;;;

;;;; ido

(ido-mode t)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

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
;; Cut and paste
;;;;;

(setq
 ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; Paste like a terminal 
(global-set-key (kbd "C-S-v") 'x-clipboard-yank)

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

;;;;;
;; Function overrides
;;;;;

(defun ask-before-closing ()
	"Prompt before closing."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
	  (save-buffers-kill-terminal)
	(save-buffers-kill-emacs))
    (message "Canceled exit")))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
