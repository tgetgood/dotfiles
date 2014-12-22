(defvar paredit-hooks
	'(
		clojure-mode-hook
		cider-repl-mode-hook
		emacs-lisp-mode-hook
	 ))

(dolist (h paredit-hooks)
	(add-hook h 'enable-paredit-mode)
	;; Always enable evil-paredit, since I don't think it does anything
	;; when evil is off.
	(add-hook h 'evil-paredit-mode))

;; And don't forget the minibuffer

(defun conditionally-enable-paredit-mode ()
	"enable paredit-mode during eval-expression"
	(if (eq this-command 'eval-expression)
			      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
