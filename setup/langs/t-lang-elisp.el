;;; -*- lexical-binding: t; -*-
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

  (t/add-company-backends-hook 'lisp-interaction-mode-hook 'company-elisp)
  (t/add-company-backends-hook 'lisp-mode-hook 'company-elisp)
  (t/add-company-backends-hook 'emacs-lisp-mode-hook 'company-elisp))

(provide 't-lang-elisp)
