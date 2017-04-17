(t/use-package flycheck-flow
  :config
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))

(t/use-package company-flow
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-flow)))

(provide 't-lang-flow)