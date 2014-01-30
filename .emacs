;;(set-face-attribute 'default nil :height 80)

;;(tool-bar-mode 0)
;;(menu-bar-mode 0)
;;(scroll-bar-mode -1)

;;(set-frame-parameter nil 'fullscreen 'fullboth)

(defun dont-kill-emacs ()
	(interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
  
(global-set-key "\C-x\C-c" 'dont-kill-emacs)


