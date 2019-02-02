(defun t-linux/init ()
  (setq t-font-size 20))

(defun t-linux/config ()
  (t/bind-in 'key-translation-map
    ;; translate norwegian os x keybindings
    "M-7" "|"
    "M-/" "\\"
    "M-8" "["
    "M-9" "]"
    "M-(" "{"
    "M-)" "}")

  ;; os x window movement
  (bind-key "s-k" 'previous-buffer)
  (bind-key "s-j" 'next-buffer)
  (bind-key "s->" 'next-multiframe-window)
  (bind-key "s-<" 'previous-multiframe-window)
  (bind-key "s-<left>" 't/smart-beginning-of-line)
  (bind-key "s-<right>" 'end-of-line)
  (bind-key "M-s-<up>" 'windmove-up)
  (bind-key "M-s-<right>" 'windmove-right)
  (bind-key "M-s-<down>" 'windmove-down)
  (bind-key "M-s-<left>" 'windmove-left)
  (bind-key "s-d" 't/split-window-right-and-move-there-dammit)
  (bind-key "s-D" 't/split-window-below-and-move-there-dammit)

  (bind-key "s-c" 'evil-yank)
  (bind-key "s-v" 'evil-paste-after)
  (bind-key "s-z" 'undo-tree-undo)
  (bind-key "s-s" 'save-buffer)
  (bind-key "s-a" 'mark-whole-buffer)
  (bind-key "s-w" 'delete-frame)
  (bind-key "s-n" 'make-frame)

  ;; s-w quits like C-x C-w
  (bind-key "s-w" #'t/delete-frame-or-hide-last-remaining-frame)

  ;; don't pop up font menu, makes new tab work in iterm2
  (bind-key "s-t" (t/lambda-i))

  ;; buffer font size adjustment
  (bind-key "s-?" (t/lambda-i (text-scale-increase 1)))
  (bind-key "s-_" (t/lambda-i (text-scale-decrease 1)))
  (bind-key "s-=" (t/lambda-i (text-scale-set 0)))

  ;; global font size adjustment
  (bind-key "s-+" 't/increase-font-size)
  (bind-key "s--" 't/decrease-font-size)
  (bind-key "s-0" 't/reset-font-size)

  ;; s-p print dialog kills emacs, so disable it..
  (bind-key "s-p" (t/lambda-i)))

(provide 't-linux)