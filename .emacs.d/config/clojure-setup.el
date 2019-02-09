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
(setq cider-repl-history-file
			(concat user-emacs-directory "transient/cider-history"))
v
;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

(setq cider-show-error-buffer nil)

;; Always prefer figwheel-main

(setq cider-default-cljs-repl 'figwheel-main)

;; figwheel-main version set globally. Not ideal, but convenient...
(add-to-list
 'cider-jack-in-cljs-dependencies
 '("com.bhauman/figwheel-main" "0.1.9"))

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

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-repl-mode-hook 'cider-mode)

(defun cider-user-ns ()
	(interactive)
	;; TODO: cljs.user in cljs repl
	(cider-repl-set-ns "user"))

;; Dev dependencies that I don't want to have to remember.

(add-to-list 'cider-jack-in-dependencies
						 '("org.clojure/tools.namespace" "0.2.11"))

(eval-after-load 'cider
	'(progn
		 (define-key cider-mode-map (kbd "C-M-r") 'cider-ns-refresh)
		 (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)
		 (define-key cider-repl-mode-map (kbd "C-c C-n") 'cider-repl-switch-to-other)

		 ;; Undoings
		 (define-key cider-mode-map (kbd "C-c M-r") nil)
		 (define-key cider-repl-mode-map (kbd "TAB") 'completion-at-point)
		 ))

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

(defun my-cider-ns-refresh ()
	(interactive)
	(cider-interactive-eval
	 "(require 'clojure.tools.namespace.repl)
		(clojure.tools.namespace.repl/refresh)"))

(define-key clojure-mode-map (kbd "C-c r") 'my-cider-ns-refresh)

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

(evil-leader/set-key-for-mode 'clojure-mode
	"c" 'cljr-clean-ns
	"m" 'cljr-add-missing-libspec
	"r" 'cider-eval-region
	"E" 'cider-load-buffer)
