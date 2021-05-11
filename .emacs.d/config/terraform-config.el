(use-package company-terraform)

(use-package terraform-mode
  :mode "\\.tf$"
  :bind (("C-c SPC" . terraform-format-buffer)))
