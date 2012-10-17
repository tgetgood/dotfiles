(set-face-attribute 'default nil :height 80)

(add-to-list 'load-path "/home/thomas/.emacs-plugins/scala-mode")
(add-to-list 'load-path "/home/thomas/.emacs-plugins/ensime/elisp")
(add-to-list 'load-path "/home/thomas/.emacs-plugins/color-theme")

(require 'scala-mode-auto)
(require 'ensime)
(require 'color-theme)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
 
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode -1)

(load-library "color-theme")
(color-theme-select)
(color-theme-ld-dark)

(kill-buffer "*Color Theme Selection*")
(set-frame-parameter nil 'fullscreen 'fullboth)

(setq ensime-sem-high-faces
  '(
   (var . (:foreground "#ff2222"))
   (val . (:foreground "#dddddd"))
   (varField . (:foreground "#ff3333"))
   (valField . (:foreground "#dddddd"))
   (functionCall . (:foreground "#84BEE3"))
   (param . (:foreground "#ffffff"))
   (class . font-lock-type-face)
   (trait . (:foreground "#084EA8"))
   (object . (:foreground "#026DF7"))
   (package . font-lock-preprocessor-face)
   ))

(windmove-default-keybindings)

(defun dont-kill-emacs ()
	(interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
  
(global-set-key "\C-x\C-c" 'dont-kill-emacs)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
