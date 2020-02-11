;;; -*- lexical-binding: t; -*-
(t/use-package flycheck
  :commands flycheck-mode
  :init
  (progn
    (setq flycheck-display-errors-function #'flycheck-display-error-messages)
    (t/add-hook '(html-mode-hook js2-mode-hook elm-mode-hook) 'flycheck-mode))
  :config
  (progn
    (t/add-to-list 't-evil-major-modes 'flycheck-error-list-mode)
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint)))
    (setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(json-jsonlist)))
    (t/after lsp-ui
      (t/after js2-mode
        (flycheck-add-mode 'javascript-eslint 'js2-mode)
        (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)))))

(t/use-package flycheck-clojure
  :pin melpa-stable
  :commands flycheck-mode
  :init
  (progn
    (t/after cider
      (comment
       (setq-default flycheck-disabled-checkers
                     (append flycheck-disabled-checkers '(clojure-cider-typed)))
       (t/add-hook 'cider-mode-hook 'flycheck-mode)
       (t/add-hook 'cider-mode-hook 'flycheck-clojure-setup)))))

(provide 't-lang-flycheck)
