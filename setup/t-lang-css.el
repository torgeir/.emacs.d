(t/use-package css-mode
  :only-standalone t
  :ensure nil
  :mode "\\.css$"
  :init
  (progn
    (setq css-indent-offset *t-indent*))
  :config
  (progn
    (bind-key "M-k" 't/css-kill-value css-mode-map)
    (dolist (hook '(css-eldoc-enable
                    turn-on-smartparens-mode
                    rainbow-mode))
      (add-hook 'css-mode-hook hook))))

(t/use-package css-eldoc
  :commands css-eldoc-enable
  :init
  (progn
    (with-eval-after-load 'company-mode
      (add-hook 'css-mode-hook #'css-eldoc-enable))))

(t/use-package less-css-mode
  :only-standalone t
  :mode "\\.less$"
  :commands less-css-mode
  :config
  (progn
    (bind-key "M-k" 't/css-kill-value css-mode-map)))

(provide 't-lang-css)
