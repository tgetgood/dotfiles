;;;;; Modified from the wiki.

;;; C-g as general purpose escape key sequence.
;;;
(defun esc-non-normal (prompt)
	"Functionality for escaping generally.  Includes exiting Evil insert state and C-g binding. "
	(cond
	 ;; If we're in one of the Evil states that defines [escape] key, return [escape] so as
	 ;; Key Lookup will use it.
	 ((or (evil-insert-state-p) (evil-replace-state-p) (evil-visual-state-p)) [escape])
	 ;; This is the best way I could infer for now to have C-c work during evil-read-key.
	 ;; Note: As long as I return [escape] in normal-state, I don't need this.
	 ;;((eq overriding-terminal-local-map evil-read-key-map) (keyboard-quit) (kbd ""))
	 (t (kbd "C-g"))))

(define-key key-translation-map (kbd "C-g") 'esc-non-normal)

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

;;;;;
;; leader keybindings
;;;;;

(evil-leader/set-key

	;;;;; Clojure bindings
	"r" 'cider-eval-region
	"E" 'cider-load-buffer

 ;;;;; Kibit
 "k" 'kibit-accept-proposed-change
 "K" 'kibit-current-file

 ;;;; clj refactor
 "c" 'cljr-clean-ns
 
	;;;;; compilation error list
 "[" 'previous-error
 "]" 'next-error

 ;;;;; indentation helpers
 "TAB" (lambda () (interactive)
				 (if (region-active-p)
						 (evil-indent (region-beginning) (region-end))
					   (evil-indent-line (point) (1+ (point)))))

 "b" 'eval-buffer
 "a" 'easy-ack

 ;;;;; Magit
 "s" 'magit-status
 "f" 'magit-fetch-all

 ;;;;; Ghetto tasklist pluging
 "t" 'tasklist-ack ; Defined in global.el
 "q" (lambda () (interactive)
			 (progn (switch-to-buffer-other-window "*Ack-and-a-half*")
							(quit-window)))
 )

(evil-ex-define-cmd "E[xplore]" (lambda () (interactive) (dired ".")))
