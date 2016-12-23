(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-special-keys nil
        which-key-sort-order 'which-key-key-order-alpha
        which-key-side-window-max-height 0.5 ; percentage height
        which-key-separator " "
        which-key-idle-delay 0.4 ; time to wait before display
        which-key-use-C-h-for-paging t
        which-key-allow-evil-operators t
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎")))
  (which-key-mode nil)
  (which-key-mode))

(provide 't-which-key)
