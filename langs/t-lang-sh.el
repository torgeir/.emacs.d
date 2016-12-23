(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :init
  (setq sh-indentation *t-indent*
        sh-basic-offset *t-indent*))

(provide 't-lang-sh)
