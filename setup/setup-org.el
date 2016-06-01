(use-package org
  :defer 2
  :init
  (setq org-directory "~/Dropbox/org")

  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
        org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")

  (setq org-src-fontify-natively t
        ;; follow links on enter, not tab
        org-return-follows-link t
        org-tab-follows-link nil
        ;; ido where possible
        org-completion-use-ido t
        ;; don't run stuff automatically on export
        org-export-babel-evaluate nil
        ;; don't prompt on every code run
        org-confirm-babel-evaluate nil
        ;; show invisibles on edit
        org-catch-invisible-edits 'show
        ;; Block parent TODOs if child is not completed
        org-enforce-todo-dependencies t
        ;; where to look for org files
        org-agenda-files '("~/Dropbox/org"
                           "~/Dropbox/org/todos")
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        ;; tag position after headings
        org-tags-column -40
        ;; force utf-8
        org-export-coding-system 'utf-8
        org-default-notes-file "~/Dropbox/org/tasks.org"
        ;; newest notes first
        org-reverse-note-order nil
        )


  (setq org-capture-templates
        '(("t" "Task"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal entry"
           entry
           (file+datetree "~/Dropbox/org/journal.org")
           "**** %U %^{Title}\n     %?")))

  :config
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

                                        ; select example
                                        ;  " %^{Tidbit type|quote|zinger|one-liner}"

  (add-hook 'org-mode-hook
            (lambda ()
              ;; yasnippet
              (make-variable-buffer-local 'yas/trigger-key)
              (org-set-local 'yas/trigger-key [tab])
              (define-key yas/keymap [tab] 'yas/next-field-group))))

(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
          (if (called-interactively-p)
              (progn
                (select-window (display-buffer buf t t))
                (org-fit-window-to-buffer)
                ;; (org-agenda-redo)
                )
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              ;; (org-agenda-redo)
              )))
      (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

(run-with-idle-timer 300 t 'jump-to-org-agenda)

(t/declare-prefix "oo" "Org"
                  "mp" 'org-mobile-push
                  "mP" 'org-mobile-pull
                  "c" 'org-capture
                  "a" 'org-agenda
                  "i" 'org-info
                  "sa" 'org-archive-set-tag
                  "st" 'org-set-tags-to
                  "t" 'org-todo-list
                  "l" 'org-timeline)

(t/declare-prefix "oos" "Tags"
                  "a" 'org-archive-set-tag
                  "t" 'org-set-tags-to)

(t/declare-prefix "ooT" "Table"
                  "Tg" 'org-table-toggle-coordinate-overlays
                  "Tf" 'org-table-formula)

(t/declare-prefix "ooC" "Clock"
                  "i" 'org-clock-in
                  "o" 'org-clock-out)

(provide 'setup-org)
