;;;;
;; Packages
;;;;

;; Package repositories
(require 'package)
(package-initialize)

; (setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-archives
			'(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
 			 ("melpa" . "http://melpa.org/packages/")
		   ("gnu" . "http://elpa.gnu.org/packages/")))

(package-refresh-contents)

(setq package-archives 
			'(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
		   ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(setq use-package-always-ensure t)

(eval-when-compile 
	(require 'use-package))
	
(use-package auto-update-package
						 :config
						 (auto-package-update-maybe))

(use-package ag)
(use-package bash-completion)

(use-package cl-generic)
(use-package cl-lib)
(use-package ido-completing-read+)
(use-package ido-select-window)

(use-package magit)

;; EVIL stuff

(use-package evil-leader)
(use-package evil-paredit)
(use-package evil-smartparens)

;; Rust Stuff

    ; cargo

;; js stuff

    vue-html-mode
    vue-mode
    xref-js2
    js2-mode
    js2-refactor
    json-mode
    nodejs-repl
    scss-mode

;; Clojure stuff

    cider
    clj-refactor
    clojure-mode-extra-font-locking
;; Python stuff
;; None of this is installed presently.
;; anaconda-mode
;; jedi
    ;; py-autopep8
    ;; company-anaconda
    ;; elpy


(defvar my-packages
  '(
    company
    company-terraform
    cyberpunk-theme
    dired-single
    docker
    docker-cli
    docker-compose-mode
    docker-tramp
    dockerfile-mode
    latex-extra
    markdown-mode
    meghanada
    popup
    reason-mode
    scion
    smex
    tagedit
    terraform-mode
    yaml-mode
    ))


; (dolist (p my-packages)
;   (when (not (package-installed-p p))
;     (package-install p)))

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

(package-install-selected-packages)

;;;;;
;; Config
;;;;;

(require 'org)

(setenv "PATH" (concat (getenv "PATH") ":/home/thomas/bin"))
(setq exec-path (append exec-path '("/home/thomas/bin")))

(add-to-list 'load-path "~/.emacs.d/config")

(load "global.el")

(load "evil-setup.el")

(load "parens-setup.el")

(load "clojure-setup.el")

(load "rust-setup.el")

(load "compilation-setup.el")

(load "org.el")

(load "markdown-setup.el")

(load "latex-custom-config.el")

(load "javascript-config.el")

(load "python-setup.el")

(load "java-setup.el")

(load "gui.el")

(add-to-list 'load-path "~/.emacs.d/downloaded")

(load "glsl-mode.el")

;;;;;
;; Thematic info
;;;;;

;;;;; This needs to override the theme settings. Which in turn need to
;;;;; come after the custom stuff.
;;;;;
;;;;; TODO: Can this be rearranged more sensibly?

(add-to-list 'after-make-frame-functions (lambda (_) (clean-ui)))

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
