;;; -*- lexical-binding: t; -*-
(use-package css-mode
  :ensure nil
  :straight nil
  :mode "\\.css$"
  :init
  (progn
    (setq css-indent-offset *t-indent*))
  :config
  (progn
    (bind-key "M-k" 't/css-kill-value css-mode-map)
    (t/add-company-backends-hook 'css-mode-hook 'company-css)
    (t/add-hook 'css-mode-hook '(turn-on-smartparens-mode rainbow-mode))))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :init
  (progn
    (t/add-hook 'css-mode-hook 'turn-on-css-eldoc)))

(use-package less-css-mode
  :mode "\\.less$"
  :commands less-css-mode
  :config
  (progn
    (bind-key "M-k" 't/css-kill-value css-mode-map)))

(provide 't-lang-css)
