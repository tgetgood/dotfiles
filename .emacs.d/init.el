;;;;
;; Packages
;;;;

;; Package repositories
(require 'package)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
												 ("org" . "https://orgmode.org/elpa/")
												 ("marmalade" . "https://marmalade-repo.org/packages/")
												 ;; ("melpa" . "http://melpa.org/packages/")
												 ;; ("gnu" . "http://elpa.gnu.org/packages/")
												 ))

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

(add-to-list 'load-path "~/.emacs.d/downloaded")

(load "glsl-mode.el")

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
   '(
     0blayout
     ag
     anaconda-mode
     autodisass-llvm-bitcode
     bash-completion
     buffer-move
     cargo
     cider
     cl-generic
     cl-lib
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
     eglot
     elpy
     evil-leader
     evil-paredit
     evil-smartparens
     flycheck-rust
     gradle-mode
     groovy-mode
     ido-completing-read+
     ido-select-window
     jedi
     js2-mode
     js2-refactor
     json-mode
     kibit-mode
     latex-extra
     lsp-metals
     lsp-mode
     lua-mode
     magit
     markdown-mode
     meacupla-theme
     meghanada
     nodejs-repl
     ob-prolog
     popup
     prolog
     py-autopep8
     racer
     reason-mode
     rustic
     scala-mode
     scion
     scss-mode
     smex
     tagedit
     terraform-mode
     typescript-mode
     use-package
     vue-html-mode
     vue-mode
     wanderlust
     xref-js2
     yaml-mode
     ))
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
