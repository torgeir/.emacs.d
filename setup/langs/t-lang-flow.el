;;; -*- lexical-binding: t; -*-

(t/use-package company-flow
  :init
  (t/after flow-minor-mode
    (t/add-hook-defun '(js2-mode-hook) t/hook-flow-company
                      (when (and (flow-minor-configured-p)
                                 (flow-minor-tag-present-p))
                        (t/add-company-backends 'company-web-html 'company-flow)))))

(t/use-package flow-minor-mode
  :init
  (t/add-hook-defun '(js2-mode-hook) t/hook-flow-web
                    (flow-minor-enable-automatically)))

(t/use-package flycheck-flow
  :init
  (t/after flycheck
    (t/add-hook-defun '(js2-mode-hook) t/hook-flow-flycheck
                      (t/when-ext "js"
                        (flycheck-add-mode 'javascript-flow 'js2-mode)
                        (flycheck-add-mode 'javascript-eslint 'js2-mode)
                        (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))))

(provide 't-lang-flow)