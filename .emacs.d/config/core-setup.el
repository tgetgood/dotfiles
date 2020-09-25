(use-package ido-completing-read+
  :commands ido-ubiquitous-mode
  :demand t
  :defines (ido-cur-item ido-cur-item)
  :config (ido-ubiquitous-mode 1))

(use-package buffer-move
  :bind (("M-H" . buf-move-left)
         ("M-L" . buf-move-right)
         ("M-J" . buf-move-down)
         ("M-K" . buf-move-up)))

(use-package smex
  :commands smex-initialize
  :bind (("M-x" . smex))
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package company
  :demand t
  :commands company-complete-common
  :bind (:map company-active-map
              ("RET" . nil)
              ("M-RET" . company-complete-selection))
  :hook (after-init . global-company-mode))

(use-package ag
  :commands (ag-project-regexp)
  :after (evil-leader)
  :demand t
  :config
  (setq ag-reuse-buffers 't)
  (setq ag-reuse-window 't)
  (defun tasklist-ag ()
	  (interactive)
	  (ag-project-regexp "@?(FIXME|TODO|HACK|OPTIMIZE|REVIEW):"))
  (evil-leader/set-key
    "a" 'ag-project-regexp
    "t" 'tasklist-ag))

(use-package bash-completion

  :commands bash-completion-setup
  :config
  (evil-define-key 'normal shell-mode-map (kbd "K") 'man)
  (bash-completion-setup)
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  ;; TODO: What do these do and are they necessary?
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
            'bash-completion-dynamic-complete))

(use-package magit
  :after (evil-leader)
  :demand t
  :config
  (evil-leader/set-key
    "s" 'magit-status
    "y" 'magit-show-refs-popup
    "b" 'magit-blame
    "B" 'magit-blame-mode))

;;;;;
;; Truly global editor config
;;;;;

;; No need for ~ files when editing
(setq create-lockfiles nil)

(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(column-number-mode)
(show-paren-mode)
(setq-default tab-width 2)
(global-auto-revert-mode 1)
(setq-default fill-column 80)

;; Don't force autocompletion
(setq company-require-match nil)
;;;; Auto-indent

(electric-indent-mode -1)

;; Org grinds to a painful crawl if you try to number its lines
(define-global-minor-mode almost-global-linum-mode linum-mode                   
	(lambda ()
		(when (not (memq major-mode
										 '(org-mode)))
			(linum-mode))))

(almost-global-linum-mode 1)

;; Is there really not a simpler way to do this?
(setq linum-format
			(lambda (line)
				(propertize
				 (format
					(let ((w (length (number-to-string
														(count-lines (point-min) (point-max))))))
						(concat "%" (number-to-string w) "d ")) line) 'face 'linum)))

;;;;;
;; Global keywords (...?)
;;;;;

;; Feeding my inline TODO list habit
;; REVIEW: does the required prefix/suffix add anything? Is
;; use/mention a big deal here?

(defvar my-warn-modes
	'(clojure-mode-hook
		emacs-lisp-mode-hook
		clojurescript-mode-hook
		clojurec-mode-hook))

(dolist (mode my-warn-modes)
	(add-hook mode
						(lambda ()
							(font-lock-add-keywords
							 nil
							 '(("\\<\\(FIXME\\|TODO\\|HACK\\|OPTIMIZE\\|REVIEW\\):"
									1 'font-lock-warning-face prepend)
								 ("\\<@\\(FIXME\\|TODO\\|HACK\\|OPTIMIZE\\|REVIEW\\)"
									1 'font-lock-warning-face prepend))))))

;;;;;
;; Auto completion
;;;;;

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-according-to-mode)))

;;;; ido

(ido-mode 1)
;; REVIEW: I think this will conflict with ido-ubiquitous-mode
;; (ido-everywhere 1)

(setq ido-case-fold t)
(setq ido-enable-flex-matching t)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

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
 select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

(defun toggle-comment-on-line ()
	"comment or uncomment current line"
	(interactive)
	(comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun ask-before-closing ()
	"Prompt before closing."
	(interactive)
	(if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
			(if (< emacs-major-version 22)
					(save-buffers-kill-terminal)
				(save-buffers-kill-emacs))
		(message "Canceled exit")))

;; Paste like a terminal
(global-set-key (kbd "C-S-v") 'clipboard-yank)

(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;;;;; Undoings
(global-set-key (kbd "M-c") nil)

;; Override default buffer view
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(global-set-key (kbd "C-x C-b") 'ibuffer) 
(global-set-key (kbd "RET") 'newline-and-indent)

;;;;;
;; Easy window navigation
;;;;
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)

;; Rebind TAB
;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(global-set-key (kbd "TAB") 'indent-or-complete)

;; Escape quits minibuffer
(define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)
