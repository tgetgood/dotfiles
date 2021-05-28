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

(use-package evil
  :demand t

  :bind (:map
         key-translation-map
         ("C-g" . esc-non-normal)
         :map evil-normal-state-map
         ("K" . describe-function)
         ("s" . #'smart-substitute)
         ("#" . (lambda (arg)
                  (interactive "P")
                  (evil-search-word-backward arg (symbol-at-point))))
         ("*" . (lambda (arg)
                  (interactive "P")
                  (evil-search-word-forward arg (symbol-at-point)))))
  :config
  (evil-mode 1)
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "K") 'describe-function)

  (evil-ex-define-cmd "E[xplore]" (lambda () (interactive) (dired ".")))

  (evil-define-operator smart-substitute (beg end type register)
    "Normal vim substitute, unless current char is an sexp delimter in which
case it's a no-op."
    :motion evil-forward-char
    (interactive "<R><x>")
    (when (not (member (char-after) '(?\" ?\( ?\[ ?\{ ?\) ?\] ?\})))
      (evil-substitute beg end type register)))

  (dolist (m my-emacs-modes)
    (add-to-list 'evil-emacs-state-modes m))

  (setq evil-insert-state-modes (remove 'shell-mode evil-insert-state-modes)))

(use-package evil-leader
  :demand t

  :commands (global-evil-leader-mode evil-leader/set-key evil-leader/set-key-for-mode)

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
    "v" 'visual-line-mode)
  (evil-leader/set-key-for-mode 'emacs-lisp-mode
    "e" 'eval-buffer))

(use-package evil-paredit
  :demand t

  :init
  (defun beginning-of-current-sexp ()
    "Moves point to beginning of current sexp. If point is at the
beginning, does not go to previous sexp. Check is rather naive."
    (when (not (member (char-before) '(?\  ?\( ?\[ ?\{ ?\n )))
      (paredit-backward)))

  (defmacro smart-wrap (command)
    `(lambda ()
       (interactive)
       (progn
         (beginning-of-current-sexp)
         (,command))))

  (defmacro smart-slurp (command)
    `(lambda ()
       (interactive)
       (progn
         (when (member (char-after) '(?\( ?\[ ?\" ?\{))
           (right-char))
			   (,command))))

  (defun wrap-double-quote (&optional argument)
    (interactive)
    (paredit-wrap-sexp argument ?\" ?\"))

  :config
  (define-key evil-normal-state-map (kbd "g l")
    (smart-slurp paredit-forward-slurp-sexp))
  (define-key evil-normal-state-map (kbd "g L") 'paredit-forward-barf-sexp)
  (define-key evil-normal-state-map (kbd "g h")
    (smart-slurp paredit-backward-slurp-sexp))
  (define-key evil-normal-state-map (kbd "g H") 'paredit-backward-barf-sexp)
  (define-key evil-normal-state-map (kbd "g k k") 'paredit-raise-sexp)
  (define-key evil-normal-state-map (kbd "g k K") 'paredit-splice-sexp)
  (define-key evil-normal-state-map (kbd "g j") 'paredit-join-sexps)
  (define-key evil-normal-state-map (kbd "g s") 'paredit-split-sexp)

  (define-key evil-normal-state-map (kbd "g (") (smart-wrap paredit-wrap-round))
  (define-key evil-normal-state-map (kbd "g [") (smart-wrap paredit-wrap-square))
  (define-key evil-normal-state-map (kbd "g {") (smart-wrap paredit-wrap-curly))
  (define-key evil-normal-state-map (kbd "g \"") (smart-wrap wrap-double-quote))

  (define-key evil-visual-state-map (kbd "g (") 'paredit-wrap-round)
  (define-key evil-visual-state-map (kbd "g [") 'paredit-wrap-square)
  (define-key evil-visual-state-map (kbd "g {") 'paredit-wrap-curly)
  (define-key evil-visual-state-map (kbd "g \"") 'wrap-double-quote))


(use-package evil-smartparens
  :commands (evil-sp-delete
             evil-sp-change
             evil-sp--override
             evil-sp--block-is-balanced
             evil-sp--new-beginning
             evil-sp--new-ending)

  :hook ((smartparens-enabled . evil-smartparens-mode)
         (smartparens-disabled . (lambda () (evil-smartparens-mode 0))))

  :config
  (sp-pair "'" "'" :actions nil)
  (define-key evil-normal-state-map (kbd "g c") 'sp-convolute-sexp)

  (evil-define-operator evil-sp-delete (beg end type register yank-handler)
    "Call `evil-delete' with a balanced region"
    (interactive "<R><x><y>")
    (if (or (evil-sp--override)
            (= beg end)
            (and (eq type 'block)
                 (evil-sp--block-is-balanced beg end)))
        (evil-delete beg end type register yank-handler)
      (condition-case nil
          (let ((new-beg (evil-sp--new-beginning beg end))
                (new-end (evil-sp--new-ending beg end)))
            (if (and (= new-end end)
                     (= new-beg beg))
                (evil-delete beg end type register yank-handler)
              (evil-delete new-beg new-end 'inclusive register yank-handler)))
        (error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                      (end (evil-sp--new-ending beg end)))
                 (evil-delete beg end type register yank-handler))))))

  (evil-define-operator evil-sp-change (beg end type register yank-handler)
    "Call `evil-change' with a balanced region"
    (interactive "<R><x><y>")
    ;; #20 don't delete the space after a word
    (when (save-excursion (goto-char end) (looking-back " " (- (point) 5)))
      (setq end (1- end)))
    (if (or (evil-sp--override)
            (= beg end)
            (and (eq type 'block)
                 (evil-sp--block-is-balanced beg end)))
        (evil-change beg end type register yank-handler)
      (condition-case nil
          (let ((new-beg (evil-sp--new-beginning beg end))
                (new-end (evil-sp--new-ending beg end)))
            (if (and (= new-end end)
                     (= new-beg beg))
                (evil-change beg end type register yank-handler)
              (evil-change new-beg new-end 'inclusive register yank-handler)))
        (error (let* ((beg (evil-sp--new-beginning beg end :shrink))
                      (end (evil-sp--new-ending beg end)))
							   (evil-change beg end type register yank-handler)))))))
  
