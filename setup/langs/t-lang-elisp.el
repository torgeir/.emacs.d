(defun t-lang-elisp/config ()
  ;; lisp
  (t/declare-prefix-for-mode 'lisp-interaction-mode "me" "Evaluate"
                             "b" 'eval-buffer
                             "e" 't/eval-region-or-last-sexp
                             "f" 'eval-defun
                             "r" 'eval-region
                             "R" 't/eval-and-replace)

  (t/declare-prefix-for-mode 'lisp-mode "me" "Evaluate"
                             "b" 'eval-buffer
                             "e" 't/eval-region-or-last-sexp
                             "f" 'eval-defun
                             "r" 'eval-region
                             "R" 't/eval-and-replace)

  (t/declare-prefix-for-mode 'emacs-lisp-mode "me" "Evaluate"
                             "b" 'eval-buffer
                             "e" 't/eval-region-or-last-sexp
                             "f" 'eval-defun
                             "r" 'eval-region
                             "p" 'eval-print-last-sexp
                             "R" 't/eval-and-replace)

  (t/add-company-backend-hook 'lisp-interaction-mode-hook 'company-elisp)
  (t/add-company-backend-hook 'lisp-mode-hook 'company-elisp)
  (t/add-company-backend-hook 'emacs-lisp-mode-hook 'company-elisp)

  (bind-key "TAB" #'t/tab-properly emacs-lisp-mode-map))

(provide 't-lang-elisp)