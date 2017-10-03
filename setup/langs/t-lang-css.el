;;; -*- lexical-binding: t; -*-
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
    (t/add-company-backend-hook 'css-mode-hook 'company-css)
    (t/add-hook 'css-mode-hook '(turn-on-smartparens-mode rainbow-mode))))

(t/use-package css-eldoc
  :commands turn-on-css-eldoc
  :init
  (progn
    (t/add-hook 'css-mode-hook 'turn-on-css-eldoc)))

(t/use-package less-css-mode
  :only-standalone t
  :mode "\\.less$"
  :commands less-css-mode
  :config
  (progn
    (bind-key "M-k" 't/css-kill-value css-mode-map)))

(provide 't-lang-css)
