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

(evil-mode 1)

;; Evil isn't always appropriate... Or is it?
;; (add-to-list 'evil-emacs-state-modes ...)
