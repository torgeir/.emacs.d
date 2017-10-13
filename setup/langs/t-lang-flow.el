;;; -*- lexical-binding: t; -*-

(t/use-package company-flow
  :init
  (with-eval-after-load 'flow-minor-mode
    (t/add-hook-defun 'web-mode-hook t/hook-flow-company
                      (when (and (flow-minor-configured-p)
                                 (flow-minor-tag-present-p))
                        (t/add-company-backends 'company-web-html 'company-tern 'company-flow)))))

(t/use-package flow-minor-mode
  :init
  (t/add-hook-defun 'web-mode-hook t/hook-flow-web
                    (flow-minor-enable-automatically)))

(use-package flycheck-flow
  :init
  (with-eval-after-load 'flycheck
    (t/add-hook-defun 'web-mode-hook t/hook-flow-flycheck
                      (t/when-ext "flow\\.js"
                        (flycheck-add-mode 'javascript-flow 'web-mode)
                        (flycheck-add-mode 'javascript-eslint 'web-mode)
                        (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))))

(provide 't-lang-flow)