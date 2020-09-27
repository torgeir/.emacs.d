;;; -*- lexical-binding: t; -*-
(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :init
  (progn
    (setq sh-indentation *t-indent*
          sh-basic-offset *t-indent*)))

(provide 't-lang-sh)
