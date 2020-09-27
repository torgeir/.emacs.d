(use-package kotlin-mode
  :mode "\\.kt$"
  :commands kotlin-mode
  :init (t/add-hook 'kotlin-mode-hook 'lsp-mode))

(provide 't-lang-kotlin)