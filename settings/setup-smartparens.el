(require 'smartparens-config)

(add-hook 'js-mode-hook 'turn-on-smartparens-mode)
(add-hook 'java-mode 'turn-on-smartparens-mode)
(add-hook 'restclient-mode-hook 'turn-on-smartparens-mode)
(add-hook 'ruby-mode 'turn-on-smartparens-mode)
(add-hook 'markdown-mode 'turn-on-smartparens-mode)

(provide 'setup-smartparens)
