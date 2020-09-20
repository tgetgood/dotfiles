(defun esc-non-normal (prompt)
  "Acts as escape in non-normal vim modes, and as C-g when in normal mode."
	(if (or (evil-insert-state-p) (evil-replace-state-p) (evil-visual-state-p))
			[escape]
		(kbd "C-g")))

;; Handy helper to figure out buffer mode names for below list.
(defun buffer-mode (buffer-or-string)
	"Returns the major mode associated with a buffer."
	(with-current-buffer buffer-or-string
		 major-mode))

;;;;;
;; Exceptions to Evil
;;;;;

(defvar my-emacs-modes
	'(cider-docview-mode
		cider-stacktrace-mode
		cider-test-report-mode
		cider-repl-mode

		nodejs-repl-mode

		haskell-error-mode
		haskell-interactive-mode

		dired-mode

		shell-mode

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
		magit-wip-save-mode))

(defun kill-the-annoying-popups ()
	(interactive)
	(dolist (buff (buffer-list))
		(let ((name (buffer-name buff)))
			(if (and (buffer-live-p buff)
							 (not (or (string-prefix-p "*cider-repl" name)
												(string-prefix-p "*cider-scratch" name)
												(string-prefix-p "*rustic-compilation" name)))
							 (or
								(string-prefix-p "*" name)
								(string-prefix-p "magit: " name)))
					(progn
						(quit-windows-on buff nil))))))

(evil-leader/set-key-for-mode 'emacs-lisp-mode
 "e" 'eval-buffer	)

(evil-leader/set-key-for-mode 'clojure-mode
	"p" (lambda ()
				(interactive)
				(cider-eval-print-last-sexp 't)))


(use-package evil
  :bind (("<escape>" . escape-non-normal)
         :map evil-normal-state-map
         ("#" . (lambda (arg)
                  (interactive "P")
                  (evil-search-word-backward arg (symbol-at-point))))
         ("*" . (lambda (arg)
                  (interactive "P")
                  (evil-search-word-forward arg (symbol-at-point)))))
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "E[xplore]" (lambda () (interactive) (dired ".")))
  (evil-define-key 'normal emacs-lisp-mode-map "K" 'describe-function)
  (dolist (m my-emacs-modes)
    (add-to-list 'evil-emacs-state-modes m)))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-key
    ;; SMerge
    "1" 'smerge-keep-current
    "2" 'smerge-keep-other
    "`" 'smerge-next
    "~" 'smerge-prev

    ;; compilation error list
    "[" 'previous-error
    "]" 'next-error

    ;; indentation helpers
    "TAB" (lambda () (interactive)
            (if (region-active-p)
                (evil-indent (region-beginning) (region-end))
              (evil-indent-line (point) (1+ (point)))))

    "q" 'kill-the-annoying-popups
    "w" 'whitespace-cleanup
    "v" 'visual-line-mode))

(use-package evil-paredit)
(use-package evil-smartparens)
