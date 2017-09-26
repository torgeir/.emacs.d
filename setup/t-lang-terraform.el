(use-package terraform-mode
  :mode "\\.tf$")

(use-package company-terraform
  :config
  (t/add-company-backend-hook 'terraform-mode-hook 'company-terraform))

(provide 't-lang-terraform)