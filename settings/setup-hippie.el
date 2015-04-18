
;; lisp-friendly
(setq hippie-expand-try-functions-list
      '(try-complete-file-name
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-dabbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-,") 'completion-at-point)
(global-set-key (kbd "C-S-.") 'hippie-expand-lines)

(provide 'setup-hippie)
