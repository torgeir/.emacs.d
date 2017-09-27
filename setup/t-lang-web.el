(t/use-package web-mode
  :only-standalone t
  :mode "\\.jsx$"
  :init
  (progn
    (setq web-mode-auto-close-style 2
          web-mode-enable-auto-quoting nil
          web-mode-markup-indent-offset *t-indent*
          web-mode-css-indent-offset *t-indent*
          web-mode-code-indent-offset *t-indent*))
  :config
  (progn
    (with-eval-after-load 'web-mode
      (bind-key "TAB" #'t/tab-properly web-mode-map)
      (defun t/jsx-hook ()
        (when-let ((is-jsx-file (string-match "\\.jsx$" (buffer-file-name))))
          ;;(tern-mode)
          ;;(js2-minor-mode)
          ))
      (add-hook 'web-mode-hook #'t/jsx-hook)
      (add-hook 'web-mode-hook ; http://web-mode.org/
                (lambda ()
                  (add-to-list 'company-dabbrev-code-modes 'web-mode)
                  (if (equal web-mode-content-type "javascript")
                      (web-mode-set-content-type "jsx"))
                  (dolist (mode '(js-mode html-mode css-mode))
                    (yas-activate-extra-mode mode)))))))

(t/use-package web-beautify
  :only-standalone t
  :commands (web-beautify-html
             web-beautify-css
             web-beautify-js)
  :init
  (progn
    (t/declare-prefix-for-mode 'web-mode  "m" "Mode" "=" 'web-beautify-html)
    (t/declare-prefix-for-mode 'css-mode  "m" "Mode" "=" 'web-beautify-css)
    (t/declare-prefix-for-mode 'json-mode "m" "Mode" "=" 'web-beautify-js)
    (t/declare-prefix-for-mode 'js2-mode  "m" "Mode" "=" 'web-beautify-js)))

(provide 't-lang-web)
