(use-package company-terraform)

(use-package terraform-mode
  :mode "\\.tf$"
  :config
  (evil-leader/set-key-for-mode 'terraform-mode
	"c" 'terraform-format-buffer))
