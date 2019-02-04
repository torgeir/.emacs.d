;;; -*- lexical-binding: t; -*-

;; lisp-friendly
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-list
        try-expand-line))

(defun t-keys/config ()
  (t/bind-in 'minibuffer-local-map "C-w" 'backward-kill-word)
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

    ;; s-w quits like C-x C-w
    "s-w" #'t/delete-frame-or-hide-last-remaining-frame
    "C-x C-c" #'t/delete-frame-or-hide-last-remaining-frame

    ;; buffer font size adjustment
    "s-?" (t/lambda (text-scale-increase 1))
    "s-_" (t/lambda (text-scale-decrease 1))
    "s-=" (t/lambda (text-scale-set 0))

    ;; global font size adjustment
    "s-+" 't/increase-font-size
    "s--" 't/decrease-font-size
    "s-0" 't/reset-font-size

    "<C-S-up>" 't/move-line-up
    "<C-S-down>" 't/move-line-down

    "M-p" 'backward-paragraph
    "M-n" 'forward-paragraph

    "C-c n" 't/cleanup-buffer-whitespace-and-indent
    "C-x C-k" 'kill-region

    "C-." 't/hippie-expand-no-case-fold
    "C-," 'completion-at-point
    "C-:" 't/hippie-expand-lines))

(provide 't-keys)
