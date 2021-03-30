(require 'semantic)

(add-hook 'c-mode-hook
          (lambda ()
            (progn
              (company-mode)
              (semantic-mode 1))))

(add-to-list 'company-backends 'company-c-headers)

(add-to-list 'company-c-headers-path-system "/usr/include/GLFW/")
(add-to-list 'company-c-headers-path-system "/usr/include/vulkan/")

(evil-leader/set-key-for-mode 'c-mode "c" 'compile)

(load "llvm-mode")

(add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode))

(evil-leader/set-key-for-mode 'llvm-mode "c" 'compile)
