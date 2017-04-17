(t/use-package markdown-mode
  :only-standalone t
  :mode "\\.\\(markdown\\|md\\)$"
  :bind (:map
         markdown-mode-map
         ("M-p" . nil)
         ("M-n" . nil)))

(provide 't-lang-markdown)
