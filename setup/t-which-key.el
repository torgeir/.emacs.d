;;; setup/t-which-key.el -*- lexical-binding: t -*-
(t/use-package which-key
  :only-standalone t
  :diminish which-key-mode
  :init
  (progn
    (setq which-key-sort-order #'which-key-prefix-then-key-order
          which-key-sort-uppercase-first nil
          which-key-add-column-padding 1
          which-key-max-display-columns nil
          which-key-min-display-lines 1
          which-key-special-keys nil
          which-key-side-window-max-height 0.5 ; percentage height
          which-key-separator " "
          which-key-idle-delay 0.4 ; time to wait before display
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

    (with-eval-after-load 'which-key
      (which-key-mode nil)
      (which-key-mode))

    (defun t/prefix-with-leader (key)
      "Prefixes `key' with `leader' and a space, e.g. 'SPC m'"
      (concat t-leader " " key))

    (defun t/prefix-with-emacs-leader (key)
      "Prefixes `key' with emacs `leader' and a space, e.g. 'C-SPC m'"
      (concat evil-leader/non-normal-prefix t-leader " " key))

    (defun t/declare-prefix (prefix name &optional key fn &rest bindings)
      "Declares which-key `prefix' and a display `name' for the prefix.
   Sets up keybindings for the prefix."
      (with-eval-after-load 'which-key
        (which-key-declare-prefixes (t/prefix-with-leader prefix) name)
        (which-key-declare-prefixes (t/prefix-with-emacs-leader prefix) name)
        (while key
          (evil-leader/set-key (concat prefix key) fn)
          (setq key (pop bindings)
                fn (pop bindings)))))

    (defun t/declare-prefix-for-mode (mode prefix name &optional key fn &rest bindings)
      "Declares which-key `prefix' and a display `name' for the prefix only in `mode`.
   Sets up keybindings for the prefix."
      (with-eval-after-load 'which-key
        (which-key-declare-prefixes-for-mode mode (t/prefix-with-leader prefix) name)
        (which-key-declare-prefixes-for-mode mode (t/prefix-with-emacs-leader prefix) name)
        (while key
          (evil-leader/set-key-for-mode mode (concat prefix key) fn)
          (setq key (pop bindings)
                fn (pop bindings)))))

    (defun t/declare-state (prefix name &optional key fn &rest bindings)
      "Micro state that temporarily overlays a new key map, kinda like hydra"
      (let ((keymap (make-sparse-keymap)))
        (while key
          (bind-key key fn keymap)
          (setq key (pop bindings)
                fn (pop bindings)))
        (evil-leader/set-key (concat prefix key)
          (lambda ()
            (interactive)
            (set-temporary-overlay-map keymap t)))))))

(provide 't-which-key)
