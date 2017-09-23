(use-package terraform-mode
  :mode "\\.tf$")

(use-package company-terraform
  :config
  (company-terraform-init))

(provide 't-lang-terraform)