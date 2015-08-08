(require 'which-key)

(progn
  ;; percentage height
  (setq which-key-side-window-max-height 0.5)

  ;; time to wait before display
  (setq which-key-idle-delay 0.8)

  (which-key-mode 0)
  (which-key-mode))

(provide 'setup-which-key)