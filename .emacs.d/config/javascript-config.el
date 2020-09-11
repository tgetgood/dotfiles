(require 'nodejs-repl)

;; CSS doesn't get its own file.
(setq css-indent-offset 2)

(setq-default indent-tabs-mode nil)

(setq js2-skip-preprocessor-directives t)
(setq js2-strict-missing-semi-warning nil)
(setq js2-include-node-externs t)
;; (setq js2-missing-semi-one-line-override nil)

;; (assq-delete-all "\\.jsm?\\'" auto-mode-alist)
;; (assq-delete-all auto-mode-alist)
;; (delq (cons "\\.json\\'" (javascript-mode)) auto-mode-alist)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(add-hook 'js2-mode-hook #'js2-refactor-mode)

(setq js-indent-level 2)

(add-hook 'js2-mode-hook
					(add-hook 'before-save-hook 'whitespace-cleanup nil t)
					(lambda () (setq indent-tabs-mode nil)))

(defun load-file-in-new-node-repl ()
	(interactive)
	(let ((buff (buffer-file-name)))
		(progn
			(if (get-buffer "*nodejs*")
					(progn
						(nodejs-repl-quit-or-cancel)
						(nodejs-repl-quit-or-cancel)))
			(nodejs-repl)
			(nodejs-repl-load-file buff)
			(insert "\n")
			(end-of-buffer))))

(add-hook 'js2-mode-hook
					(lambda ()
						(define-key nodejs-repl-mode-map (kbd "TAB")
							'nodejs-repl-complete-from-process)
						(define-key js2-mode-map (kbd "C-c M-j") 'nodejs-repl)
						(define-key js2-mode-map (kbd "C-c C-k") 'load-file-in-new-node-repl)
						(define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

(defun set-eslint-standard ()
	"Set flycheck to use standard for linting"
	(interactive)
	(setq flycheck-javascript-eslint-executable "standard --verbose"))

;;;;; Vue.js

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))


