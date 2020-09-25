;; This is useful for workin CamelCase tokens, like names of Java classes
;; This is actually pretty annoying.
;; (add-hook 'clojure-mode-hook 'subword-mode)

(use-package clojure-mode
  :init
  (defun paredit-space-for-reader-conditional (endp delim)
    "Do not insert a space between #? and ("
    (or endp
        (cond ((eq (char-syntax delim) ?\()
               (not (looking-back (regexp-quote "#?") 2 nil)))
              (t t))))

  :hook ((clojure-mode . enable-paredit-mode)
         (clojurec-mode . (lambda ()
                            (add-to-list
                             'paredit-space-for-delimiter-predicates
                             'paredit-space-for-reader-conditional)))
         (clojure-mode . (lambda ()
                            (progn
                              (add-hook 'before-save-hook 'whitespace-cleanup nil t)
                              (set (make-local-variable
                                    'comment-auto-fill-only-comments)
                                   t)))))
  :mode ("\\.edn$"
         "\\.clj$"
         "\\.boot$"
         ("\\.cljc$". clojurec-mode)
         ("\\.cljs$" . clojurescript-mode))

  :config
  (evil-define-key 'normal clojure-mode-map (kbd "K") 'cider-doc)
  (define-key evil-normal-state-map (kbd "g :") 'clojure-toggle-keyword-string))

(use-package clojure-mode-extra-font-locking)

(use-package cider
  :commands (cider-eval-print-last-sexp)
  :hook ((cider-mode . eldoc-mode)
         (cider-repl-mode . enable-paredit-mode))

  :bind (:map cider-mode-map
         ("C-c C-o" . cider-repl-clear-buffer)
         ("C-M-r" . cider-ns-refresh)

         :map cider-repl-mode-map
	       ("C-c C-n" . cider-repl-switch-to-other)
         ("C-c C-a" . cider-switch-to-last-clojure-buffer)
         ("TAB" . completion-at-point))

  :config
  (evil-leader/set-key-for-mode 'clojure-mode
	"p" (lambda ()
				(interactive)
				(cider-eval-print-last-sexp 't)))

  (set 'cider-repl-display-help-banner nil)

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

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  (setq cider-show-error-buffer nil)

  ;; Always prefer figwheel-main

  (setq cider-default-cljs-repl 'figwheel-main)

  (setq cider-figwheel-main-default-options "dev")
  
  (evil-leader/set-key-for-mode 'clojure-mode
    "r" 'cider-eval-region
    "e" 'cider-load-buffer)

  (evil-leader/set-key-for-mode 'clojurescript-mode
    "r" 'cider-eval-region
    "e" 'cider-load-buffer)

  (evil-leader/set-key-for-mode 'clojurec-mode
    "r" 'cider-eval-region
	  "e" 'cider-load-buffer))

(use-package clj-refactor
  :commands (clj-refactor-mode cljr-add-keybindings-with-prefix)
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c C-v")))

  :config
  (setq cljr-warn-on-eval nil)
  (setq cljr-magic-requires nil)
  (setq cljr-favor-prefix-notation nil)
  (setq cljr-eagerly-build-asts-on-startup nil)

  (define-key evil-normal-state-map (kbd "g k h") 'cljr-splice-sexp-killing-backward)
  (define-key evil-normal-state-map (kbd "g k l") 'cljr-splice-sexp-killing-forward)

  (evil-leader/set-key-for-mode 'clojure-mode
    "c" 'cljr-clean-ns
    "m" 'cljr-add-missing-libspec)

  (evil-leader/set-key-for-mode 'clojurescript-mode
    "c" 'cljr-clean-ns
    "m" 'cljr-add-missing-libspec)

  (evil-leader/set-key-for-mode 'clojurec-mode
    "c" 'cljr-clean-ns
    "m" 'cljr-add-missing-libspec))
