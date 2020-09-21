;;;;
;; Packages
;;;;

(setenv "PATH" (concat (getenv "PATH") ":/home/thomas/bin"))
(setq exec-path (append exec-path '("/home/thomas/bin")))

(add-to-list 'load-path "~/.emacs.d/config")

;; Package repositories
(require 'package)

 (when (not package--initialized)
   (package-initialize))

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
 			 ("melpa" . "http://melpa.org/packages/")
		   ("gnu" . "http://elpa.gnu.org/packages/")))

(when (not package-archive-contents)
	(package-refresh-contents))

(setq package-archives 
			'(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
		   ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  	(package-refresh-contents)
	(package-install 'use-package))

;; This isn't optimised for bytecode. We'll see if that ever matters.
(require 'use-package)

(setq use-package-always-ensure t)

(load "ui-setup.el")

(load "evil-setup.el")

(load "core-setup.el")

;; (load "parens-setup.el")

(load "org-setup.el")


;; TODO: Configure this properly or don't bother installing it.
;; (use-package dired-single)

;; (use-package company-terraform)
;; (use-package terraform-mode)
;; (use-package docker)
;; (use-package docker-cli)
;; (use-package docker-compose-mode)
;; (use-package docker-tramp)
;; (use-package dockerfile-mode)

;; (use-package tex
;;   :ensure auctex
;;   :hook ((LaTeX-mode . visual-line-mode)
;;          (LaTeX-mode . flyspell-mode)
;;          (LaTeX-mode . LaTeX-math-mode)
;;          (LaTeX-mode . turn-on-reftex))
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)
;;   (setq TeX-PDF-mode t)
;;   (setq reftex-plug-into-AUCTeX t)
;;   (setq-default TeX-master nil))

;; (use-package markdown-mode
;;   :hook (markdown-mode
;;          .
;;          (lambda ()
;;            (progn
;;              (auto-fill-mode 1)
;;              (add-hook 'before-save-hook 'whitespace-cleanup nil t)))))

;; (use-package reason-mode)

;; Machine Generated

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-names-vector
;;    ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
;;  '(browse-url-browser-function 'browse-url-default-browser)
;;  '(browse-url-generic-program nil)
;;  '(cider-offer-to-open-cljs-app-in-browser nil)
;;  '(custom-safe-themes
;;    '("addfaf4c6f76ef957189d86b1515e9cf9fcd603ab6da795b82b79830eed0b284" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" default))
;;  '(fci-rule-color "#383838")
;;  '(ido-case-fold t)
;;  '(ido-enable-flex-matching t)
;;  '(magit-bury-buffer-function 'magit-mode-quit-window)
;;  '(package-selected-packages
;;    '(groovy-mode meghanada xref-js2 js2-refactor js2-mode nodejs-repl vue-mode tagedit scss-mode use-package smex rustic reason-mode magit ido-ubiquitous ido-select-window ido-completing-read+ evil-smartparens evil-paredit evil-leader eglot dockerfile-mode docker-compose-mode docker-cli docker cyberpunk-theme company-terraform buffer-move bash-completion autodisass-llvm-bitcode auto-package-update ag))
;;  '(sp-autoskip-closing-pair t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(flymake-error ((nil (:foreground "#ef0000" :underline nil :weight bold))))
;;  '(flymake-note ((nil (:distant-foreground "black" :box nil :underline (:color "#555555" :style wave)))))
;;  '(flymake-warning ((nil (:foreground "#FBDE2D" :underline nil :weight bold))))
;;  '(italic ((t (:slant normal :weight normal :height 80 :width normal :foundry "DejaVu" :family "Sans Mono"))))
;;  '(mmm-default-submode-face ((t nil)))
;;  '(mode-line ((t (:background "#333333" :foreground "#4c83ff"))))
;;  '(mode-line-highlight ((t nil)))
;;  '(mode-line-inactive ((t (:background "#1A1A1A" :foreground "#4D4D4D")))))

;;;;;
;; Config
;;;;;


;; (load "clojure-setup.el")

;; (load "rust-setup.el")

;; TODO: What does this do?
;; (load "compilation-setup.el")

;; (load "javascript-config.el")

;; (load "python-setup.el")

;; (load "java-setup.el")


;; (add-to-list 'load-path "~/.emacs.d/downloaded")

;; (load "glsl-mode.el")

;; (use-package auto-package-update
;;   :config
;;   (auto-package-update-maybe))

;;;;; I think these things are dead weight

;; (use-package cl-generic)

;; (use-package cl-lib)

;; REVIEW: I don't think I'm actually using this anywhere
;; (use-package popup)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("20a8ec387dde11cc0190032a9f838edcc647863c824eed9c8e80a4155f8c6037" default))
 '(fci-rule-color "#383838")
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-gnus org-habit org-id org-tempo ol-w3m))
 '(package-selected-packages
   '(magit bash-completion ag company smex buffer-move ido-select-window ido-completing-read+ ido-ubiquitous evil-leader evil cyberpunk-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
