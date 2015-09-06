(require 'smartparens-config)
(require 'paredit)

(defvar paren-hooks '( clojure-mode-hook
											 cider-repl-mode-hook
											 cider-repl-mode-hook
											 schema-mode-hook
											 lisp-mode-hook
											 emacs-lisp-mode-hook))

(dolist (h paren-hooks)
	(add-hook h 'enable-paredit-mode))

;; And the minibuffer

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

;;;;; Let's try and use paredit + evil-smartparens. This will be fun

(add-hook 'paredit-mode-hook 'smartparens-strict-mode)
(add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)


(add-hook 'smartparens-disabled-hook (lambda () (evil-smartparens-mode 0)))

;; Disable some of smartparens

(sp-pair "'" nil :actions :rem)
(sp-pair "\"" nil :actions :rem)

;; Helper to wrap sexps more intuitively

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

;; Extensions to evil-smartparens

(evil-define-operator smart-substitute (beg end type register)
	"Normal vim substitute, unless current char is an sexp delimter in
which case it's a no-op."
	:motion evil-forward-char
	(interactive "<R><x>")
	(when (not (member (char-after) '(?\" ?\( ?\[ ?\{ ?\) ?\] ?\})))
		(evil-substitute beg end type register)))

(define-key evil-normal-state-map (kbd "s") #'smart-substitute)

;;;;;

(defun wrap-double-quote (&optional argument)
	(interactive)
	(paredit-wrap-sexp argument ?\" ?\"))


;; Paredit keymap REVIEW: should I not be using the g prefix here?

(define-key evil-normal-state-map (kbd "g l") (smart-slurp paredit-forward-slurp-sexp))
(define-key evil-normal-state-map (kbd "g L") 'paredit-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "g h") (smart-slurp paredit-backward-slurp-sexp))
(define-key evil-normal-state-map (kbd "g H") 'paredit-backward-barf-sexp)
(define-key evil-normal-state-map (kbd "g k k") 'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd "g k h") 'paredit-splice-sexp-killing-backward)
(define-key evil-normal-state-map (kbd "g k l") 'paredit-splice-sexp-killing-forward)
(define-key evil-normal-state-map (kbd "g j") 'paredit-join-sexp)
(define-key evil-normal-state-map (kbd "g s") 'paredit-split-sexp)

(define-key evil-normal-state-map (kbd "g (") (smart-wrap paredit-wrap-round))
(define-key evil-normal-state-map (kbd "g [") (smart-wrap paredit-wrap-square))
(define-key evil-normal-state-map (kbd "g {") (smart-wrap paredit-wrap-curly))
(define-key evil-normal-state-map (kbd "g \"") (smart-wrap wrap-double-quote))

(define-key evil-normal-state-map (kbd "M-h") 'paredit-backward)
(define-key evil-normal-state-map (kbd "M-l") 'paredit-forward)
(define-key evil-normal-state-map (kbd "M-k") 'paredit-backward-up)
(define-key evil-normal-state-map (kbd "M-K") 'paredit-forward-up)
(define-key evil-normal-state-map (kbd "M-j") 'paredit-forward-down)
(define-key evil-normal-state-map (kbd "M-J") 'paredit-backward-down)

(define-key evil-visual-state-map (kbd "g (") 'paredit-wrap-round)	
(define-key evil-visual-state-map (kbd "g [") 'paredit-wrap-square)	
(define-key evil-visual-state-map (kbd "g {") 'paredit-wrap-curly)
(define-key evil-visual-state-map (kbd "g \"") 'wrap-double-quote)

	

	

