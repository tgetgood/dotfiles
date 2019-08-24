;;;;
;; Packages
;;;;

;; Package repositories
(require 'package)

(add-to-list 'package-archives
						 '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
						 '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
						 '("marmalade" . "https://marmalade-repo.org/packages/"))

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
	(package-refresh-contents))

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


;; Machine Generated


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(browse-url-generic-program nil)
 '(cider-offer-to-open-cljs-app-in-browser nil)
 '(custom-safe-themes
	 (quote
		("addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default)))
 '(fci-rule-color "#383838")
 '(ido-case-fold t)
 '(ido-enable-flex-matching t)
 '(magit-bury-buffer-function (quote magit-mode-quit-window))
 '(org-modules
	 (quote
		(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
	 (quote
		(
		 0blayout
		 ag
		 anaconda-mode
		 autodisass-llvm-bitcode
		 bash-completion
		 buffer-move
		 cargo
		 cider
		 clj-refactor
		 clojure-mode-extra-font-locking
		 company
		 company-anaconda
		 company-terraform
		 cyberpunk-theme
		 dired-single
		 docker
		 docker-cli
		 docker-compose-mode
		 docker-tramp
		 dockerfile-mode
		 elpy
		 evil-leader
		 evil-paredit
		 evil-smartparens
		 gradle-mode
		 groovy-mode
		 ido-select-window
		 ido-ubiquitous
		 jedi
		 js2-mode
		 js2-refactor
		 json-mode
		 kibit-mode
		 latex-extra
		 lua-mode
		 magit
		 markdown-mode
		 meacupla-theme
		 meghanada
		 nodejs-repl
		 popup
		 py-autopep8
		 racer
		 reason-mode
		 rust-mode
		 scion
		 scss-mode
		 smex
		 tagedit
		 terraform-mode
		 wanderlust
		 xref-js2
		 yaml-mode
		 )))
 '(sp-autoskip-closing-pair t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant normal :weight normal :height 80 :width normal :foundry "DejaVu" :family "Sans Mono")))))

;;;;;
;; Thematic info
;;;;;

(load-theme 'cyberpunk t)
;; (load-theme 'leuven t)

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
