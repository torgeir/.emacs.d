;;; -*- lexical-binding: t; -*-
(t/use-package web-mode
  :mode "\\.jsx?$"
  :init
  (progn
    (t/after web-mode
      (setq web-mode-enable-html-entities-fontification t)
      (t/add-hook-defun 'web-mode-hook t/hook-web ; http://web-mode.org/
                        (setq-local web-mode-auto-close-style 2)
                        (setq-local web-mode-enable-auto-quoting nil)
                        (setq-local web-mode-markup-indent-offset *t-indent*)
                        (setq-local web-mode-css-indent-offset *t-indent*)
                        (setq-local web-mode-code-indent-offset *t-indent*)

                        (js2-minor-mode)

                        (tern-mode)
                        (t/add-company-backends 'company-web-html 'company-tern)

                        (add-to-list 'company-dabbrev-code-modes 'web-mode)

                        (when (equal web-mode-content-type "javascript")
                          (web-mode-set-content-type "jsx"))

                        (dolist (mode '(js-mode html-mode css-mode))
                          (yas-activate-extra-mode mode))))))

(t/use-package web-beautify
  :commands (web-beautify-html web-beautify-css web-beautify-js)
  :init
  (progn
    (t/declare-prefix-for-mode 'web-mode  "m" "Mode" "=" 'web-beautify-html)
    (t/declare-prefix-for-mode 'css-mode  "m" "Mode" "=" 'web-beautify-css)
    (t/declare-prefix-for-mode 'json-mode "m" "Mode" "=" 'web-beautify-js)))

(provide 't-lang-web)
