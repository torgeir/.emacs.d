(require 'guide-key)

(setq guide-key/guide-key-sequence t)
(guide-key-mode 1)

(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 1) ;; seconds

(provide 'setup-guide-key)