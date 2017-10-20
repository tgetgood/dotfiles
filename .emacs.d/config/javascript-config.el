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

(add-hook 'js2-mode-hook
					(lambda ()
						(define-key js2-mode-map (kbd "C-c M-j") 'nodejs-repl)
						(define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
						(define-key js2-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
						(define-key js2-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
						(define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
						(define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
