;;; -*- lexical-binding: t; -*-
(defun t-keys/vars ()

  ;; lisp-friendly
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-all-abbrevs
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-expand-list
          try-expand-line)))

(defun t-keys/config ()
  (bind-key "<M-S-up>" 't/move-line-up)
  (bind-key "<M-S-down>" 't/move-line-down)

  (bind-key "M-p" 'backward-paragraph)
  (bind-key "M-n" 'forward-paragraph)
  (bind-key "C-w" 'backward-kill-word minibuffer-local-map) ; minibuffer
  (bind-key "C-c n" 't/cleanup-buffer-whitespace-and-indent)
  (bind-key "C-x C-k" 'kill-region)
  (bind-key "C-x C-c" #'t/delete-frame-or-hide-last-remaining-frame)

  ;; completion
  (bind-key "C-." 't/hippie-expand-no-case-fold)
  (bind-key "C-," 'completion-at-point)
  (bind-key "C-:" 't/hippie-expand-lines))

(provide 't-keys)
