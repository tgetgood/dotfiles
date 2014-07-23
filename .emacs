(require 'cl)

;; emacs native settings
(menu-bar-mode -1)
(global-linum-mode 1)
(column-number-mode)
(show-paren-mode)
(setq-default tab-width 2)
(setq js-indent-level 2)
(global-auto-revert-mode 1)
	
;; Windmove customization

(global-set-key (kbd "C-c h")  'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k")    'windmove-up)
(global-set-key (kbd "C-c j")  'windmove-down)

;; How in hell?? (set-fill-column 80)

;; handy stolen methods

(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (member major-mode '(emacs-lisp-mode lisp-mode
                                                      clojure-mode    scheme-mode
                                                      haskell-mode    ruby-mode
                                                      rspec-mode      python-mode
                                                      c-mode          c++-mode
                                                      objc-mode       latex-mode
                                                      plain-tex-mode))
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))


(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)  

  
;; Taken cut-and-paste from prelude; I just wanted to understand, 
;; Maybe I'll switch

  (defvar prelude-packages
    '(ack-and-a-half auctex clojure-mode coffee-mode deft expand-region
		     gist groovy-mode haml-mode haskell-mode inf-ruby
		     magit markdown-mode paredit projectile python
		     sass-mode rainbow-mode scss-mode solarized-theme
		     volatile-highlights yaml-mode yari zenburn-theme)
    "A list of packages to ensure are installed at launch.")

  (defun prelude-packages-installed-p ()
    (loop for p in prelude-packages
	  when (not (package-installed-p p)) do (return nil)
	  finally (return t)))

  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-packages)
      (when (not (package-installed-p p))
	(package-install p))))

  (provide 'prelude-packages)
;; prelude-packages.el ends here
)

;; package config

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)

;; Cider

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(require 'clojure.tools.namespace.repl)
    (clojure.tools.namespace.repl/refresh)"))

(define-key clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

;; clj-refactor

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-v")
                               ))

;; auto-complete? 

(require 'auto-complete)
(global-auto-complete-mode t)


(ido-mode)

(require 'undo-tree)
(global-undo-tree-mode t)

;; I should just learn to be careful about not doing this..

(defun dont-kill-emacs ()
	(interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
  
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
'(custom-safe-themes (quote ("c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "968d1ad07c38d02d2e5debffc5638332696ac41af7974ade6f95841359ed73e3" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'cyberpunk t)

