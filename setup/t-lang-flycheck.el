(t/use-package flycheck
  :commands flycheck-mode
  :only-standalone t
  :init
  (progn
    (setq flycheck-display-errors-function #'flycheck-display-error-messages))
  :config
  (progn
    (t/add-to-list t-evil-major-modes 'flycheck-error-list-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(javascript-jshint)))
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers '(json-jsonlist)))
    (add-hook 'html-mode-hook 'flycheck-mode)
    (add-hook 'web-mode-hook 'flycheck-mode)))

(t/use-package flycheck-clojure
  :pin melpa-stable
  :commands flycheck-mode
  :init
  (progn
    (with-eval-after-load 'cider
      (add-hook 'cider-mode-hook 'flycheck-mode)))
  :config
  (progn
    (add-to-list 'flycheck-checkers 'clojure-cider-eastwood)))

(provide 't-lang-flycheck)
