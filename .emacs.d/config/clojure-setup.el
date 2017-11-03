;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;;;;;
;; Cider
;;;;;

(require 'cider)

(set 'cider-repl-display-help-banner nil)

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook #'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect nil)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)
(setq cider-auto-jump-to-error t)

;; Fast docs
(setq cider-prompt-for-symbol nil)

;; More font lock
(setq cider-font-lock-dynamically '(macro core function var))

;; Where to store the cider history.
(setq cider-repl-history-file (concat user-emacs-directory "transient/cider-history"))

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx$" . clojurex-mode))
(add-to-list 'auto-mode-alist '("\\.cljc$" . clojurec-mode))
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))

(defun cider-refresh ()
	(interactive)
	(cider-interactive-eval
	 (format "(require 'clojure.tools.namespace.repl)
(clojure.tools.namespace.repl/refresh)")))

(defun cider-refresh-on-save ()
	(lambda ()
		(add-hook 'after-save-hook
							(lambda ()
								(if (and (boundp 'cider-mode) cider-mode)
										(cider-refresh))))))

(add-hook 'clojure-mode-hook 'cider-refresh-on-save)
(add-hook 'clojurec-mode-hook 'cider-refresh-on-save)

(defun cider-user-ns ()
	(interactive)
	;; TODO: cljs.user in cljs repl
	(cider-repl-set-ns "user"))

(eval-after-load 'cider
	'(progn
		 (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
		 (define-key cider-mode-map (kbd "C-M-r") 'cider-refresh)
		 (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
		 (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;;;;;
;; Cider keys
;;;;;

(define-key cider-repl-mode-map (kbd "C-c C-a") 'cider-switch-to-last-clojure-buffer)

;;;;;
;; clj-refactor
;;;;;

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
															 (clj-refactor-mode 1)
															 (cljr-add-keybindings-with-prefix "C-c C-v")))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-repl-mode-hook 'cider-mode)

(setq cljr-warn-on-eval nil)
(setq cljr-magic-requires nil)
(setq cljr-favor-prefix-notation nil)

;;;;;
;; Compilation tasks
;;;;;

(defun lein-run ()
	(interactive)
	(compile "lein trampoline run" t)
	(switch-to-buffer-other-window "*compilation*")
	(rename-buffer "*lein-run*"))

;; (set-variable 'cljsbuild-compile-command "lein trampoline cljsbuild auto dev")

(defun cljsbuild ()
	(interactive)
	(cljsbuild-start "lein trampoline cljsbuild auto dev"))

;; Reset repl

(defun cider-ns-refresh ()
	(interactive)
	(cider-interactive-eval
	 "(require 'clojure.tools.namespace.repl)
		(clojure.tools.namespace.repl/refresh)"))

(define-key clojure-mode-map (kbd "C-c r") 'cider-ns-refresh)

;;;;;
;; Evil customisations
;;;;;

(define-key evil-normal-state-map "K" 'cider-doc)

;;;;;
;; Figwheel - Cider Setup
;;;;;

(setq cider-cljs-lein-repl
			"(do (require '[figwheel-sidecar.repl-api :as fig])
					 (fig/start-figwheel!)
					 (fig/cljs-repl))")

(defun paredit-space-for-reader-conditional (endp delim)
	"Do not insert a space between #? and ("
	(or endp
			(cond ((eq (char-syntax delim) ?\()
						 (not (looking-back (regexp-quote "#?") 2 nil)))
						(else t))))

(add-hook 'clojurec-mode-hook
					(lambda ()
						(add-to-list
						 'paredit-space-for-delimiter-predicates
						 'paredit-space-for-reader-conditional)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; comment-sexp
;;;;;
;; Stolen from:
;; http://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uncomment-sexp (&optional n)
	"Uncomment a sexp around point."
	(interactive "P")
	(let* ((initial-point (point-marker))
				 (inhibit-field-text-motion t)
				 (p)
				 (end (save-excursion
								(when (elt (syntax-ppss) 4)
									(re-search-backward comment-start-skip
																			(line-beginning-position)
																			t))
								(setq p (point-marker))
								(comment-forward (point-max))
								(point-marker)))
				 (beg (save-excursion
								(forward-line 0)
								(while (and (not (bobp))
														(= end (save-excursion
																		 (comment-forward (point-max))
																		 (point))))
									(forward-line -1))
								(goto-char (line-end-position))
								(re-search-backward comment-start-skip
																		(line-beginning-position)
																		t)
								(ignore-errors
									(while (looking-at-p comment-start-skip)
										(forward-char -1)))
								(point-marker))))
		(unless (= beg end)
			(uncomment-region beg end)
			(goto-char p)
			;; Indentify the "top-level" sexp inside the comment.
			(while (and (ignore-errors (backward-up-list) t)
									(>= (point) beg))
				(skip-chars-backward (rx (syntax expression-prefix)))
				(setq p (point-marker)))
			;; Re-comment everything before it.
			(ignore-errors
				(comment-region beg p))
			;; And everything after it.
			(goto-char p)
			(forward-sexp (or n 1))
			(skip-chars-forward "\r\n[:blank:]")
			(if (< (point) end)
					(ignore-errors
						(comment-region (point) end))
				;; If this is a closing delimiter, pull it up.
				(goto-char end)
				(skip-chars-forward "\r\n[:blank:]")
				(when (eq 5 (car (syntax-after (point))))
					(delete-indentation))))
		;; Without a prefix, it's more useful to leave point where
		;; it was.
		(unless n
			(goto-char initial-point))))

(defun comment-sexp--raw ()
	"Comment the sexp at point or ahead of point."
	(pcase (or (bounds-of-thing-at-point 'sexp)
						 (save-excursion
							 (skip-chars-forward "\r\n[:blank:]")
							 (bounds-of-thing-at-point 'sexp)))
		(`(,l . ,r)
		 (goto-char r)
		 (skip-chars-forward "\r\n[:blank:]")
		 (save-excursion
			 (comment-region l r))
		 (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
	"Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
	(interactive "P")
	(if (or (elt (syntax-ppss) 4)
					(< (save-excursion
							 (skip-chars-forward "\r\n[:blank:]")
							 (point))
						 (save-excursion
							 (comment-forward 1)
							 (point))))
			(uncomment-sexp n)
		(dotimes (_ (or n 1))
			(comment-sexp--raw))))

(define-key cider-mode-map (kbd "C-;") 'comment-or-uncomment-sexp)
(define-key emacs-lisp-mode-map (kbd "C-;") 'comment-or-uncomment-sexp)
