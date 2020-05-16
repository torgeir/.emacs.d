;;; -*- lexical-binding: t; -*-
(use-package terraform-mode
  :mode "\\.tf$"
  :config (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(t/use-package company-terraform
  :after terraform-mode
  :config
  (t/add-company-backends-hook 'terraform-mode-hook 'company-terraform))

(provide 't-lang-terraform)