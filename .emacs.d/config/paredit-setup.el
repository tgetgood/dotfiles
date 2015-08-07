(require 'smartparens-config)

(defvar paren-hooks
	'(
		clojure-mode-hook
		cider-repl-mode-hook
		emacs-lisp-mode-hook
		cider-repl-mode-hook 
	 ))

(dolist (h paren-hooks)
	(add-hook h 'smartparens-mode))

;; Always enable evil-paredit, since I don't think it does anything
;; when evil is off.
(add-hook 'smartparens-enabled-hook 'evil-smartparens-mode)
(add-hook 'smartparens-enabled-hook 'smartparens-strict-mode)

;; And don't forget the minibuffer

(defun conditionally-enable-paredit-mode ()
	"enable paredit-mode during eval-expression"
	(if (eq this-command 'eval-expression)
			(smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
