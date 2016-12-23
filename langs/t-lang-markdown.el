(use-package markdown-mode
  :mode "\\.\\(markdown\\|md\\)$"
  :bind (:map
         markdown-mode-map
         ("M-p" . nil)
         ("M-n" . nil)))

(provide 't-lang-markdown)
