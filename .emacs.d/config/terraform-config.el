(use-package company-terraform)

(use-package terraform-mode
  :mode "\\.tf$"
  :bind (:map terraform-mode-map
              ("C-c SPC" . terraform-format-buffer)))
