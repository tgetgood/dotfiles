(require 'nodejs-repl)
(require 'js2-highlight-vars)

(setq js2-skip-preprocessor-directives t)
(setq js2-strict-missing-semi-warning nil)
(setq js2-include-node-externs t)
;; (setq js2-missing-semi-one-line-override nil)

(remove-alist 'auto-mode-alist "\\.jsm?\\'")
(remove-alist 'auto-mode-alist "\\.json\\'")
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(add-hook 'js2-mode-hook
					(lambda ()
						(progn
							(flycheck-mode 1)
							(if (featurep 'js2-highlight-vars)
									(js2-highlight-vars-mode)))))

(defun load-file-in-new-node-repl ()
	(interactive)
	(let ((buff (buffer-file-name)))
		(progn
			(if (get-buffer "*nodejs*")
					(progn
						(nodejs-repl-quit-or-cancel)
						(nodejs-repl-quit-or-cancel)))
			(nodejs-repl)
			(nodejs-repl-load-file buff))))

(add-hook 'js2-mode-hook
					(lambda ()
						(define-key js2-mode-map (kbd "C-c M-j") 'nodejs-repl)
						(define-key js2-mode-map (kbd "C-c C-k") 'load-file-in-new-node-repl)
						(define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
