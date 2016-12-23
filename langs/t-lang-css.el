(use-package css-mode
  :ensure nil
  :mode "\\.css$"
  :bind (:map
         css-mode-map
         ("M-k" . t/css-kill-value))
  :init
  (setq css-indent-offset *t-indent*)
  :config
  (dolist (hook '(css-eldoc-enable
                  turn-on-smartparens-mode
                  rainbow-mode))
    (add-hook 'css-mode-hook hook)))

(use-package css-eldoc
  :commands css-eldoc-enable
  :init
  (add-hook 'css-mode-hook #'css-eldoc-enable))

(use-package less-css-mode
  :mode "\\.less$"
  :commands less-css-mode
  :bind (:map
         css-mode-map
         ("M-k" . t/css-kill-value)))

(provide 't-lang-css)
