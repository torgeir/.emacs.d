(use-package web-mode
  :mode "\\.jsx$"
  :init
  (setq web-mode-auto-close-style 2
        web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset *t-indent*
        web-mode-css-indent-offset *t-indent*
        web-mode-code-indent-offset *t-indent*)
  :config
  (bind-key "TAB" #'t/tab-properly web-mode-map)
  (add-hook 'web-mode-hook (lambda ()
                             (let ((is-jsx-file (not (null (string-match "\\.jsx$" (buffer-file-name))))))
                               (when is-jsx-file
                                 (js2-minor-mode)))))
  (add-hook 'web-mode-hook ; http://web-mode.org/
            (lambda ()
              (add-to-list 'company-dabbrev-code-modes 'web-mode)
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx"))
              (dolist (mode '(js-mode html-mode css-mode))
                (yas-activate-extra-mode mode)))))

(use-package web-beautify
  :commands (web-beautify-html
             web-beautify-css
             web-beautify-js)
  :init
  (t/declare-prefix-for-mode 'web-mode  "m" "Mode" "=" 'web-beautify-html)
  (t/declare-prefix-for-mode 'css-mode  "m" "Mode" "=" 'web-beautify-css)
  (t/declare-prefix-for-mode 'json-mode "m" "Mode" "=" 'web-beautify-js)
  (t/declare-prefix-for-mode 'js2-mode  "m" "Mode" "=" 'web-beautify-js))

(provide 't-lang-web)
