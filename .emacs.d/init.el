;;;;
;; Packages
;;;;

(setenv "PATH" (concat (getenv "PATH") ":/home/thomas/bin"))
(setq exec-path (append exec-path '("/home/thomas/bin")))

(add-to-list 'load-path "~/.emacs.d/config")

;; Package repositories
(require 'package)
(package-initialize)

;;;;;
;; This requires some explanation for future me:
;;
;; rustic only exists on unstable melpa, but I don't want to install anything
;; else from there. At the same time, I don't want to refresh the package
;; contents more than once per loading emacs.
;;
;; So, I refresh all archives, remove melpa unstable, and then add it back
;; temporarily in "rust-setup.el" just to install rustic. Ugh
;;;;;
(setq package-archives
			'(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
 			 ("melpa" . "http://melpa.org/packages/")
		   ("gnu" . "http://elpa.gnu.org/packages/")))

(when (not package-archive-contents)
	(package-refresh-contents))

(setq package-archives 
			'(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
		   ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(setq use-package-always-ensure t)

;; This isn't optimised for bytecode. We'll see if that ever matters.
(require 'use-package)
	
(use-package auto-package-update
  :config
  (auto-package-update-maybe))

(use-package cl-generic)

(use-package cl-lib)

;; REVIEW: I don't think I'm actually using this anywhere
;; (use-package popup)

(use-package cyberpunk-theme)

;; Modes, settings, and key bindings that apply to emacs core. 
;; Plus random stuff that I don't know where else to put...
(load "global.el")

(use-package ido-completing-read+)

(use-package ido-select-window
  :commands (ido-select-window)
  :bind (("C-x o" . ido-select-window)))

(use-package buffer-move
  :bind (("M-H" . buf-move-left)
         ("M-L" . buf-move-right)
         ("M-J" . buf-move-down)
         ("M-K" . buf-move-up)))

(use-package company
  :bind (:map company-active-map
              ("RET" . nil)
              ("M-RET" . company-complete-selection))
  :hook (after-init . global-company-mode))

(use-package smex
  :bind (("M-x" . smex))
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(load "evil-setup.el")

(use-package ag
  :commands (ag-project-regexp)
  :after (evil-leader)
  :config
  (setq ag-reuse-buffers 't)
  (setq ag-reuse-window 't)
  (defun tasklist-ag ()
	  (interactive)
	  (ag-project-regexp "@?(FIXME|TODO|HACK|OPTIMIZE|REVIEW):"))
  (evil-leader/set-key
    "a" 'ag-project-regexp
    "t" 'tasklist-ag))

(use-package bash-completion
  :config
  (bash-completion-setup)
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  ;; TODO: What do these do and are they necessary?
  (add-hook 'shell-dynamic-complete-functions
            'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions
            'bash-completion-dynamic-complete))

(use-package magit
  :after (evil-leader)
  :config
  (evil-leader/set-key
    "s" 'magit-status
    "y" 'magit-show-refs-popup
    "b" 'magit-blame
    "B" 'magit-blame-mode))

;; TODO: Configure this properly or don't bother installing it.
;; (use-package dired-single)

(use-package company-terraform)
(use-package terraform-mode)
(use-package docker)
(use-package docker-cli)
(use-package docker-compose-mode)
(use-package docker-tramp)
(use-package dockerfile-mode)

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . turn-on-reftex))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq reftex-plug-into-AUCTeX t)
  (setq-default TeX-master nil))

(use-package latex-extra)

(use-package markdown-mode
  :hook (markdown-mode
         .
         (lambda ()
           (progn
             (auto-fill-mode 1)
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)))))

(use-package reason-mode)

;; Machine Generated

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(browse-url-browser-function 'browse-url-default-browser)
 '(browse-url-generic-program nil)
 '(cider-offer-to-open-cljs-app-in-browser nil)
 '(custom-safe-themes
   '("addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default))
 '(fci-rule-color "#383838")
 '(ido-case-fold t)
 '(ido-enable-flex-matching t)
 '(magit-bury-buffer-function 'magit-mode-quit-window)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-gnus org-habit org-id org-tempo ol-w3m))
 '(package-selected-packages
   '(0blayout ag anaconda-mode autodisass-llvm-bitcode bash-completion buffer-move cargo cider cl-generic cl-lib clj-refactor clojure-mode-extra-font-locking company company-anaconda company-terraform cyberpunk-theme dired-single docker docker-cli docker-compose-mode docker-tramp dockerfile-mode eglot elpy evil-leader evil-paredit evil-smartparens flycheck-rust gradle-mode groovy-mode ido-completing-read+ ido-select-window jedi js2-mode js2-refactor json-mode kibit-mode latex-extra lsp-mode lua-mode magit markdown-mode meacupla-theme meghanada nodejs-repl popup py-autopep8 racer reason-mode rustic scion scss-mode smex tagedit terraform-mode use-package vue-html-mode vue-mode wanderlust xref-js2 yaml-mode))
 '(sp-autoskip-closing-pair t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((nil (:foreground "#ef0000" :underline nil :weight bold))))
 '(flymake-note ((nil (:distant-foreground "black" :box nil :underline (:color "#555555" :style wave)))))
 '(flymake-warning ((nil (:foreground "#FBDE2D" :underline nil :weight bold))))
 '(italic ((t (:slant normal :weight normal :height 80 :width normal :foundry "DejaVu" :family "Sans Mono"))))
 '(mmm-default-submode-face ((t nil)))
 '(mode-line ((t (:background "#333333" :foreground "#4c83ff"))))
 '(mode-line-highlight ((t nil)))
 '(mode-line-inactive ((t (:background "#1A1A1A" :foreground "#4D4D4D")))))

;;;;;
;; Config
;;;;;

(load "org.el")

;; (load "parens-setup.el")

;; (load "clojure-setup.el")

(load "rust-setup.el")

;; TODO: What does this do?
(load "compilation-setup.el")

(load "javascript-config.el")

(load "python-setup.el")

(load "java-setup.el")

(load "gui.el")

(add-to-list 'load-path "~/.emacs.d/downloaded")

(load "glsl-mode.el")

;;;;;
;; Thematic info
;;;;;

(add-to-list 'after-make-frame-functions (lambda (_) (clean-ui)))
;; TODO: Use display-graphic-p to make this run nicely in the console.

(defun dark ()
	(interactive)
	(load-theme 'cyberpunk t)
	(clean-ui))

(defun light ()
	(interactive)
	(load-theme 'leuven t)
	(clean-ui))

(load-theme 'cyberpunk t)
;; (load-theme 'leuven t)
