(use-package org-mac-iCal
  :commands (org-mac-iCal))
(use-package org-mac-link
  :commands (org-mac-grab-link))

(use-package org
  :defer 2
  :init
  (setq org-directory (if is-mac
                       "~/Dropbox/org"
                       "c:/Users/torgth/Dropbox \(Personlig\)/org"))

  (setq org-mobile-directory (if is-mac
                                "~/Dropbox/Apps/MobileOrg"
                                "c:/Users/torgth/Dropbox \(Personlig\)/Apps/MobileOrg")
        org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))

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
        org-agenda-files `(,(concat org-directory "/tasks.org")
                           ,(concat org-directory "/todos/home.org")
                           ,(concat org-directory "/todos/bekk.org")
                           ,(concat org-directory "/todos/kom.org")
                           ,(concat org-directory "/todos/rsi.org")
                           ,(concat org-directory "/todos/datainn.org"))
        ;; default duration of events
        org-agenda-default-appointment-duration 60
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        ;; tag position after headings
        org-tags-column -60
        ;; force utf-8
        org-export-coding-system 'utf-8
        org-default-notes-file (concat org-directory "/tasks.org")
        ;; newest notes first
        org-reverse-note-order t
        )

  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CANCELLED")))

  (setq org-modules '(org-mouse
                      ;; TODO error when loading these two
                      ;org-eval
                      ;org-expiry
                      ))
  (eval-after-load 'org
    '(org-load-modules-maybe t))

  (eval-after-load 'org
    '(require 'ox-md))

  (setq org-capture-templates
        '(("t" "Task"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("j" "Journal entry"
           entry
           (file+datetree (concat org-directory "/journal.org"))
           "**** %U %^{Title}\n     %?")))

                                        ; select example
                                        ;  " %^{Tidbit type|quote|zinger|one-liner}"


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

  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode 1)
              ;; yasnippet
              (make-variable-buffer-local 'yas/trigger-key)
              (org-set-local 'yas/trigger-key [tab])
              (define-key yas/keymap [tab] 'yas-next-field-or-maybe-expand)))

  ;; fix completion dissapearing
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-capf))
  (defun add-pcomplete-to-capf ()
    (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
  (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

  ;; show week numbers in calendar
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7 :foreground "VioletRed2")
  (copy-face 'default 'calendar-iso-week-header-face)
  (set-face-attribute 'calendar-iso-week-header-face nil :height 0.7 :foreground "VioletRed4")
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d" (car (calendar-iso-from-absolute (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face)
        calendar-intermonth-header (propertize "WK" 'font-lock-face 'calendar-iso-week-header-face)))

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
                  "c" 'org-capture
                  "e" 'org-export-dispatch
                  "g" 'org-mac-grab-link
                  "a" 'org-agenda
                  "i" 'org-info)

(t/declare-prefix "oom" "Mobile"
                  "p" 'org-mobile-push
                  "P" 'org-mobile-pull)

(t/declare-prefix "ool" "Links"
                  "s" 'org-store-link
                  "i" 'org-insert-link)

(t/declare-prefix "oot" "Tags"
                  "a" 'org-archive-set-tag
                  "t" 'org-set-tags-to)

(t/declare-prefix "ooT" "Table"
                  "Tg" 'org-table-toggle-coordinate-overlays
                  "Tf" 'org-table-formula)

(t/declare-prefix "ooC" "Clock"
                  "i" 'org-clock-in
                  "o" 'org-clock-out)

(provide 't-org)
