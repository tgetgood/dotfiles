
;;;;
;; Packages
;;;;

;; Package repositories
(require 'package)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
	     '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
    paredit
    cider
    clj-refactor
    clojure-quick-repls
    kibit-mode
    slamhound
    flycheck-clojure

    ;; cljs
    cljsbuild-mode
    
    ;; Haskell
    haskell-mode
    scion
		
    ;;Evil
    evil
    evil-paredit
		
    ;; Themes
    cyberpunk-theme

    ;; Web
    scss-mode
    markdown-mode
    ;; HTML pseudo-paredit
    tagedit
    
    ;; Completion
    company
    bash-completion

    ;; Comms
    wanderlust
    jabber
		
    ;; Misc
    flycheck-pos-tip
    undo-tree
    ack-and-a-half
    ido-ubiquitous
    smex
    magit
    ))

(dolist (p packages)
  (when (not (package-installed-p p))
        (package-install p)))

;;;;;
;; Config
;;;;;

(setenv "PATH" (concat (getenv "PATH") ":/home/thomas/bin"))
(setq exec-path (append exec-path '("/home/thomas/bin")))

(add-to-list 'load-path "~/.emacs.d/config")

(load "global.el")

(load "evil-setup.el")

(load "paredit-setup.el")

(load "clojure-setup.el")

;; Indentation exceptions for clojure.
(load "clojure-indentation.el")

(load "compilation-setup.el")

(load "grunt-setup.el")

(load "workflows.el")
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
 '(custom-safe-themes
	 (quote
		("f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default)))
 '(fci-rule-color "#383838"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(italic ((t (:slant normal :weight normal :height 90 :width normal :foundry "DejaVu" :family "Sans Mono")))))

;;;;;
;; Thematic info
;;;;;

(load-theme 'cyberpunk t)		

;;;;; This needs to override the theme settings. Which in turn need to
;;;;; come after the custom stuff.
;;;;;
;;;;; TODO: Can this be rearranged more sensibly?

(if (window-system)
	(load "gui.el")
	(add-hook 'server-visit-hook
						(lambda ()
							(load "gui.el"))))
	
