;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;;;;;
;; Cider
;;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file (concat user-emacs-directory "transient/cider-history"))

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljx.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
	(interactive)
	(cider-load-current-buffer)
	(let ((ns (cider-current-ns)))
		(cider-repl-set-ns ns)
		(cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
		(cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
	(interactive)
	(cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
	(interactive)
	(cider-repl-set-ns "user"))

(eval-after-load 'cider
	'(progn
		 (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
		 (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
		 (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
		      (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;;;;;
;; clj-refactor
;;;;;

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-v")
                               ))

(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'cider-repl-mode-hook 'cider-mode)

;;;;;
;; Kibit
;;;;;

;;; Stolen verbatim from the README

;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
         '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))


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

;;;;;
;; Evil customisations
;;;;;

(define-key evil-normal-state-map "K" 'cider-doc)

