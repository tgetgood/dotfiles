(add-to-list 'load-path "/home/thomas/.emacs-plugins/scala-mode")
(add-to-list 'load-path "/home/thomas/.emacs-plugins/ensime/src/main/elisp")

(require 'scala-mode-auto)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
