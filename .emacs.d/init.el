;;;;
;; Packages
;;;;

;; Package repositories
(require 'package)

(add-to-list 'package-archives
						 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
						 '("org" . "http://orgmode.org/elpa/") t)


;; Lkoad and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
	(package-refresh-contents))


;; Everything *should* be in here. There's probably stuff installed
;; via package-list-packages... Use C-h v package-activated-list to
;; see what's installed. Too bad that lists dependencies as well.
(defvar packages
	'(
		;; Clojure
		clojure-mode
		clojure-mode-extra-font-locking
		cider
		clj-refactor
		kibit-helper
		paredit

		edn
		yasnippet
		multiple-cursors

		;; cljs
		cljsbuild-mode

		;; Haskell
		haskell-mode
		scion

		;;Evil
		evil
		evil-leader
		evil-smartparens

		;; Themes
		cyberpunk-theme
		meacupla-theme

		;; Web
		scss-mode
		markdown-mode

		;; HTML pseudo-paredit
		tagedit

		;; LaTeX
		auctex
		auctex-latexmk
		;; Completion
		company
		bash-completion

		;; Comms
		wanderlust
		jabber

		;; Misc
		flycheck-pos-tip
		inflections
		hydra
		undo-tree
		ag
		ido-ubiquitous
		smex
		buffer-move
		lua-mode

		;; Git
		gh
		magit
		))

(dolist (p packages)
	(when (not (package-installed-p p))
				(package-install p)))

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

;; Indentation exceptions for clojure.
(load "clojure-indentation.el")

(load "compilation-setup.el")

(load "grunt-setup.el")

(load "workflows.el")

(load "org.el")

(load "latex-custom-config.el")

;;;;;
;; Machine Generated
;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(browse-url-browser-function (quote browse-url-chrome))
 '(browse-url-generic-program nil)
 '(custom-safe-themes
	 (quote
		("f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default)))
 '(fci-rule-color "#383838")
 '(ido-case-fold t)
 '(ido-enable-flex-matching t)
 '(magit-bury-buffer-function (quote magit-mode-quit-window))
 '(org-modules
	 (quote
		(org-bbdb org-bibtex org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
	 (quote
		(latex-extra cider-eval-sexp-fu aggressive-indent 0blayout alchemist elixir-mode dired-single lua-mode exec-path-from-shell buffer-move cider wanderlust tagedit smex scss-mode scion popup meacupla-theme markdown-mode magit kibit-mode kibit-helper jabber ido-ubiquitous haskell-mode gh flycheck-pos-tip evil-smartparens evil-paredit evil-leader cyberpunk-theme company clojure-mode-extra-font-locking cljsbuild-mode clj-refactor bash-completion ag ack-and-a-half)))
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
;(load-theme 'meacupla t)

;;;;; This needs to override the theme settings. Which in turn need to
;;;;; come after the custom stuff.
;;;;;
;;;;; TODO: Can this be rearranged more sensibly?

(add-to-list 'after-make-frame-functions (lambda (_) (load "gui.el")))
