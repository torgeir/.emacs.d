(require 'evil)

(setq evil-search-module 'evil-search
      evil-want-C-u-scroll t
      evil-want-C-w-in-emacs-state t)

(evil-mode t)
(setq evil-default-state 'emacs)

(provide 'setup-evil)
