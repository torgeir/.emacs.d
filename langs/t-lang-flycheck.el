(use-package flycheck
  :commands flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (use-package flycheck-pos-tip
    :init
    (eval-after-load 'flycheck
      '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package flycheck-clojure
  :pin melpa-stable
  :commands flycheck-mode
  :init
  (add-hook 'cider-mode-hook (lambda () (flycheck-mode 1)))

  :config
  (add-to-list 'flycheck-checkers 'clojure-cider-eastwood))


(provide 't-lang-flycheck)
