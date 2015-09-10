(use-package org
  :defer t
  :config
  '(progn
     ;; use cider instead of slime (default)
     (setq org-babel-clojure-backend 'cider)
     (eval-after-load "ob-clojure"
       '(defun org-babel-execute:clojure (body params)
          "Execute a block of Clojure code with Babel."
          (let ((expanded (org-babel-expand-body:clojure body params))
                result)
            (require 'cider)
            (let ((result-params (cdr (assoc :result-params params))))
              (setq result
                    (nrepl-dict-get
                     (nrepl-sync-request:eval expanded)
                     (if (or (member "output" result-params)
                             (member "pp" result-params))
                         "out"
                       "value"))))

            (org-babel-result-cond (cdr (assoc :result-params params))
              result
              (condition-case nil
                  result
                  (error result))))))

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (clojure . t)
        (python . t)
        (ruby . t)
        (js . t)
        (sh . t)))

     (setq org-src-fontify-natively t)
     ;; follow links on enter
     (setq org-return-follows-link t)
     ;; ido where possible
     (setq org-completion-use-ido t)
     ;; don't run stuff automatically on export
     (setq org-export-babel-evaluate nil)
     ;; don't prompt on every code run
     (setq org-confirm-babel-evaluate nil)
     ))

(provide 'setup-org)
