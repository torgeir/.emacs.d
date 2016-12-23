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

(defun t/prefix-with-leader (key)
  "Prefixes `key' with `leader' and a space, e.g. 'SPC m'"
  (concat t-leader " " key))

(defun t/declare-prefix (prefix name &optional key fn &rest bindings)
  "Declares which-key `prefix' and a display `name' for the prefix.
   Sets up keybindings for the prefix."
  (which-key-declare-prefixes (t/prefix-with-leader prefix) name)
  (let ((init-prefix prefix))
    (while key
      (evil-leader/set-key (concat init-prefix key) fn)
      (setq key (pop bindings)
            fn (pop bindings)))))

(defun t/declare-prefix-for-mode (mode prefix name &optional key fn &rest bindings)
  "Declares which-key `prefix' and a display `name' for the prefix only in `mode`.
   Sets up keybindings for the prefix."
  (which-key-declare-prefixes-for-mode mode (t/prefix-with-leader prefix) name)
  (let ((init-prefix prefix))
    (while key
      (evil-leader/set-key-for-mode mode (concat init-prefix key) fn)
      (setq key (pop bindings)
            fn (pop bindings)))))

(defun t/declare-state (prefix name &optional key fn &rest bindings)
  "Micro state that temporarily overlays a new key map, kinda like hydra"
  (lexical-let* ((keymap (make-sparse-keymap))
                 (init-prefix prefix))
    (while key
      (bind-key key fn keymap)
      (setq key (pop bindings)
            fn (pop bindings)))
    (evil-leader/set-key
      (concat init-prefix key)
      (lambda ()
        (interactive)
        (set-temporary-overlay-map keymap t)))))

(provide 't-which-key)
