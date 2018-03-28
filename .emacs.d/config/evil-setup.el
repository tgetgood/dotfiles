;;;;; Modified from the wiki.

;;; C-g as general purpose escape key sequence.
;;;
(defmacro esc-non-normal (save)
	`(lambda (prompt)
		"Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
		(cond
		 ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
		 ;; Key Lookup will use it.
		 ((or (evil-insert-state-p) (evil-replace-state-p) (evil-visual-state-p))
			(progn
				(if ,save (save-buffer))
				[escape]))
		 ;; This is the best way I could infer for now to have C-c work during
		 ;; evil-read-key.
		 ;; Note: As long as I return [escape] in normal-state, I don't need this.
		 ;; ((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit)
		 ;; (kbd ""))
		 (t (kbd "C-g")))))

(define-key key-translation-map (kbd "<f12>") (esc-non-normal nil))

(global-evil-leader-mode)
(evil-mode 1)

;; Handy helper to figure out buffer mode names for below list.
(defun buffer-mode (buffer-or-string)
	"Returns the major mode associated with a buffer."
	(with-current-buffer buffer-or-string
		 major-mode))

;;;;;
;; Exceptions to Evil
;;;;;

(defvar my-emacs-modes
	'(
		cider-docview-mode
		cider-stacktrace-mode
		cider-test-report-mode
		cider-repl-mode

		nodejs-repl-mode

		haskell-error-mode
		haskell-interactive-mode

		dired-mode

		magit-auto-revert-mode
		magit-blame-mode
		magit-branch-manager-mode
		magit-cherry-mode
		magit-commit-mode
		magit-diff-mode
		magit-key-mode
		magit-log-edit-mode
		magit-log-mode
		magit-mode
		magit-process-mode
		magit-reflog-mode
		magit-status-mode
		magit-wazzup-mode
		magit-wip-save-mode
		))

(dolist (m my-emacs-modes)
	(add-to-list 'evil-emacs-state-modes m))

(define-key evil-normal-state-map (kbd "M-.") nil)

(define-key evil-normal-state-map (kbd "*")
	(lambda (arg)
		(interactive "P")
		(evil-search-word-forward arg (symbol-at-point))))

(define-key evil-normal-state-map (kbd "#")
	(lambda (arg)
		(interactive "P")
		(evil-search-word-backward arg (symbol-at-point))))

;; Aggressive save toggle

(define-key evil-normal-state-map (kbd "g a")
	(lambda ()
		(interactive)
		(define-key key-translation-map (kbd "<f12>") (esc-non-normal nil))))

(define-key evil-normal-state-map (kbd "g A")
	(lambda ()
		(interactive)
		(define-key key-translation-map (kbd "<f12>") (esc-non-normal 't))))

;;;;;
;; leader keybindings
;;;;;

(defvar my-annoying-window-list
	(list "*Help*" "*ag search*" "*cider-doc*" "*Backtrace*"))

(defun kill-the-annoying-popups ()
	(interactive)
	(dolist (buff (buffer-list))
		(let ((name (buffer-name buff)))
			(if (and (buffer-live-p buff)
							 (or
								(member name my-annoying-window-list)
								(string-prefix-p "*magit" name)))
					(progn
						(quit-windows-on buff nil))))))

(evil-leader/set-key

	;; Clojure bindings
	"r" 'cider-eval-region
	"E" 'cider-load-buffer

	;; Kibit
 "k" 'kibit-accept-proposed-change
 "K" 'kibit-current-file

 ;; clj refactor
 "c" 'cljr-clean-ns
 "m" 'cljr-add-missing-libspec

 ;; compilation error list
 "[" 'previous-error
 "]" 'next-error

 ;; indentation helpers
 "TAB" (lambda () (interactive)
				 (if (region-active-p)
						 (evil-indent (region-beginning) (region-end))
						 (evil-indent-line (point) (1+ (point)))))

 "e" 'eval-buffer
 "a" 'ag-project-regexp

 ;; Magit
 "s" 'magit-status
 "y" 'magit-show-refs-popup
 "b" 'magit-blame
 "B" 'magit-blame-mode

 ;; Smerge bindings
 "1" 'smerge-keep-current
 "2" 'smerge-keep-other
 "`" 'smerge-next
 "~" 'smerge-prev

 ;; org
 "g" 'org-agenda
 "l" 'org-store-link

 ;; Ghetto tasklist pluging
 "t" 'tasklist-ag ; Defined in global.el
 "q" 'kill-the-annoying-popups

 ;; Everyday stuff
 "w" 'whitespace-cleanup
 "v" 'visual-line-mode
 )

(evil-ex-define-cmd "E[xplore]" (lambda () (interactive) (dired ".")))

(evil-define-key 'normal emacs-lisp-mode-map "K" 'describe-function)
(evil-define-key 'normal clojure-mode-map "K" 'cider-doc)
