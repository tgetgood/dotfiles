;;;;; Modified from the wiki.

;;; C-c as general purpose escape key sequence.
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

(defvar my-emacs-modes
	'(
		cider-docview-mode
		cider-stacktrace-mode
		haskell-error-mode
		haskell-interactive-mode
		
		))

(evil-mode 1)

;; Handy helper to figure out buffer mode names for below list. 
(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
     major-mode))

(dolist (m my-emacs-modes) 
	(add-to-list 'evil-emacs-state-modes m))

;; Evil isn't always appropriate... Or is it?
;; (add-to-list 'evil-emacs-state-modes ...)
