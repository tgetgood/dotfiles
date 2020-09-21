(setenv "PATH" (concat (getenv "PATH") ":/home/thomas/bin"))
(setq exec-path (append exec-path '("/home/thomas/bin")))

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/downloaded")

;; Package repositories
(require 'package)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/"))

      package-archive-priorities '(("melpa-stable" . 100)
                                   ("gnu" . 50)
                                   ("org" . 40)
                                   ("melpa" . 0)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; This isn't optimised for bytecode. We'll see if that ever matters.

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(setq use-package-always-ensure t)

(load "ui-setup.elc")

(load "parens-setup.elc")

(load "evil-setup.elc")

(load "core-setup.elc")

(load "org-setup.elc")

(load "clojure-setup.elc")

(load "rust-setup.elc")

(load "javascript-config.elc")

;; TODO: What does this do?
;; (load "compilation-setup.el")

(load "python-setup.elc")

(load "java-setup.elc")

(load "glsl-mode.elc")

;; TODO: Configure this properly or don't bother installing it.
;; (use-package dired-single)

;;;;; Homeless packages

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

(use-package markdown-mode
  :hook (markdown-mode
         .
         (lambda ()
           (progn
             (auto-fill-mode 1)
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)))))

(use-package reason-mode)

(use-package auto-package-update
  :commands auto-package-update-maybe
  :config
  (auto-package-update-maybe))

(use-package auto-compile
  :demand t
  :commands auto-compile-on-load-mode
  :config (auto-compile-on-load-mode))

;;;;; Machine Generated

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
   '(auto-compile reason-mode auctex dockerfile-mode docker-compose-mode docker-cli docker company-terraform auto-package-update groovy-mode meghanada clj-refactor cider clojure-mode-extra-font-locking clojure-mode evil-smartparens evil-paredit magit bash-completion ag company smex buffer-move ido-select-window ido-completing-read+ ido-ubiquitous evil-leader evil cyberpunk-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
