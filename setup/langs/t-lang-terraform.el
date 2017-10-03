;;; -*- lexical-binding: t; -*-
(use-package terraform-mode
  :mode "\\.tf$")

(t/use-package company-terraform
  :after terraform-mode
  :config
  (t/add-company-backend-hook 'terraform-mode-hook 'company-terraform))

(provide 't-lang-terraform)