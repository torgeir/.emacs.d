(defun t-linux/init ()
  (setq t-font-size 20)

  (t/bind-in 'key-translation-map
    ;; translate norwegian os x keybindings
    "M-7" "|"
    "M-/" "\\"
    "M-8" "["
    "M-9" "]"
    "M-(" "{"
    "M-)" "}")

  ;; os x window movement
  (t/bind-in 'global-map
    "s-k" 'previous-buffer
    "s-j" 'next-buffer
    "s->" 'next-multiframe-window
    "s-<" 'previous-multiframe-window
    "s-<left>" 't/smart-beginning-of-line
    "s-<right>" 'end-of-line
    "M-s-<up>" 'windmove-up
    "M-s-<right>" 'windmove-right
    "M-s-<down>" 'windmove-down
    "M-s-<left>" 'windmove-left
    "s-d" 't/split-window-right-and-move-there-dammit
    "s-D" 't/split-window-below-and-move-there-dammit

    "s-c" 'evil-yank
    "s-v" 'evil-paste-after
    "s-z" 'undo-tree-undo
    "s-s" 'save-buffer
    "s-a" 'mark-whole-buffer
    "s-w" 'delete-frame
    "s-n" 'make-frame

    ;; s-w quits like C-x C-w
    "s-w" #'t/delete-frame-or-hide-last-remaining-frame

    ;; buffer font size adjustment
    "s-?" (t/lambda (text-scale-increase 1))
    "s-_" (t/lambda (text-scale-decrease 1))
    "s-=" (t/lambda (text-scale-set 0))

    ;; global font size adjustment
    "s-+" 't/increase-font-size
    "s--" 't/decrease-font-size
    "s-0" 't/reset-font-size))


(provide 't-linux)