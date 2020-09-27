;;; -*- lexical-binding: t; -*-
(use-package terraform-mode
  :mode "\\.tf$"
  :init
  (t/add-hook-defun 'terraform-mode-hook t-hook-terraform
                    (terraform-format-on-save-mode)
                    (aggressive-indent-mode -1)))

(use-package company-terraform
  :after terraform-mode
  :init
  (t/add-company-backends-hook 'terraform-mode-hook 'company-terraform))

(provide 't-lang-terraform)