(require 'semantic)

(add-hook 'c-mode-hook
          (lambda ()
            (progn
              (semantic-mode 1))))
