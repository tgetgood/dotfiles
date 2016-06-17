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
	(cider-interactive-eval (format "(bop.utils/refresh-repl)")))

(defun cider-user-ns ()
	(interactive)
	(cider-repl-set-ns "boot.user"))

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

