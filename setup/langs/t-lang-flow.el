;;; -*- lexical-binding: t; -*-
(t/use-package flycheck-flow
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))

(t/use-package company-flow)

(provide 't-lang-flow)