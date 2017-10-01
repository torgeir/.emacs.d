(t/use-package markdown-mode
  :only-standalone t
  :mode "\\.\\(markdown\\|md\\)$"
  :config
  (progn
    (unbind-key "M-p" markdown-mode-map)
    (unbind-key "M-n" markdown-mode-map)))

(provide 't-lang-markdown)
