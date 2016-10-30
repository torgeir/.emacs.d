(use-package org
  :ensure org-plus-contrib
  :defer 2
  :bind (:map
         org-src-mode-map
         ("C-c C-c" . org-edit-src-exit))
  :init
  (setq org-directory (if is-mac
                          "~/Dropbox/org"
                        "c:/Users/torgth/Dropbox \(Personlig\)/org"))

  (setq org-mobile-directory (if is-mac
                                 "~/Dropbox/Apps/MobileOrg"
                               "c:/Users/torgth/Dropbox \(Personlig\)/Apps/MobileOrg")
        org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))

  (setq org-startup-indented t ; turn on org-indent-mode
        org-return-follows-link t
        org-tab-follows-link nil
        org-hide-emphasis-markers t
        org-completion-use-ido t
        org-blank-before-new-entry '((heading . t) (plain-list-item . t))
        ;; number of empty lines after heading needed to show visible newline between headings
        org-cycle-separator-lines 2
        org-catch-invisible-edits 'show
        org-enforce-todo-dependencies t ; block parent TODOs if child is not completed
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        org-tags-column -60 ; tag position after headings
        org-export-coding-system 'utf-8
        org-default-notes-file (concat org-directory "/tasks.org")
        org-special-ctrl-k t ; don't clear tags, etc
        org-adapt-indentation t ; move text to align with heading bullets

        org-reverse-note-order t ; newest notes first
        org-log-done t ; log when todos are completed
        org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "CANCELLED")))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-export-babel-evaluate nil ; don't run stuff automatically on export
        org-edit-src-content-indentation 0)

  (setq org-agenda-default-appointment-duration 60
        org-agenda-window-setup 'only-window ; delete other windows when showing agenda
        org-agenda-files (t/find-org-file-recursively org-directory) ; where to look for org files
        org-agenda-skip-scheduled-if-done nil ; prevent showing done scheduled items
        org-agenda-custom-commands `(
                                     ("b" tags-todo "book" ',(t/find-org-file-recursively org-directory))
                                     ("v" tags-todo "video" ((org-agenda-files ',(t/find-org-file-recursively org-directory))))
                                     ("C" todo "CANCELLED" ((org-agenda-files ',(t/find-org-file-recursively org-directory))))
                                     ))
  (setq org-modules '(org-mouse
                      ;; TODO error when loading these two
                      ;;org-eval
                      ;;org-expiry
                      ))

  (eval-after-load 'org
    '(org-load-modules-maybe t))

  (eval-after-load 'org
    '(require 'ox-md))

  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<p class=\"author\">%a (%e)</p>\n<p class=\"date\">%T</p>")))

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
  (defvar org-babel-clojure-nrepl-timeout 20)
  (eval-after-load "ob-clojure"
    '(defun org-babel-execute:clojure (body params)
       "Execute a block of Clojure code with Babel."
       (let ((expanded (org-babel-expand-body:clojure body params))
             result)
         (require 'cider)
         (let ((result-params (cdr (assoc :result-params params))))
           (setq result
                 (nrepl-dict-get
                  (let ((nrepl-sync-request-timeout org-babel-clojure-nrepl-timeout))
                    (nrepl-sync-request:eval expanded (cider-current-connection) (cider-current-session)))
                  (if (or (member "output" result-params)
                          (member "pp" result-params))
                      "out"
                    "value"))))
         (org-babel-result-cond (cdr (assoc :result-params params))
           result
           (condition-case nil
               result
             (error result))))))

  (use-package ob-restclient
    :after org)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (python . t)
     (ruby . t)
     (js . t)
     (latex . t)
     (sh . t)
     (dot . t)
     (restclient . t)))

  (add-hook 'org-babel-after-execute-hook 't/org-fix-inline-images)

  (add-hook 'org-mode-hook
            (lambda ()
              (bind-key "C-c C-c" 'org-edit-src-exit org-src-mode-map)
              (bind-key "C-c C-k" 'org-edit-src-abort org-src-mode-map)
              (org-display-inline-images t t)
              (visual-line-mode 1) ; wrap long lines
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
                (org-agenda-redo))
            (with-selected-window (display-buffer buf)
              (org-fit-window-to-buffer)
              (org-agenda-redo))))
      (call-interactively 'org-agenda-list)))
  (let ((buf (get-buffer "*Calendar*")))
    (unless (get-buffer-window buf)
      (org-agenda-goto-calendar)
      (other-window 1))))

(run-with-idle-timer 300 t 'jump-to-org-agenda)

(use-package org-alert
  :after org
  :config
  (setq alert-default-style 'osx-notifier
        org-alert-interval 3600)
  (org-alert-enable)

  (defun alert-osx-notifier-notify (info)
    "Overriding this function of `org-alert' fixes `osx-notifier'."
    (apply #'call-process "osascript" nil nil nil "-e" (list (format "display notification %S with title %S"
                                                                     (alert-encode-string (plist-get info :message))
                                                                     (alert-encode-string (plist-get info :title)))))
    (alert-message-notify info)))

(use-package org-bullets
  :after org
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-mac-iCal
  :after org
  :commands (org-mac-iCal))

(use-package org-mac-link
  :after org-plus-contrib
  :ensure org-plus-contrib
  :commands (org-mac-grab-link))

(use-package weather-metno
  :after org
  :config
  (setq weather-metno-location-name "Trondheim, Norway"
        weather-metno-location-latitude 63.427
        weather-metno-location-longitude 10.391))

(use-package calendar-norway
  :config
  (setq calendar-holidays
        (append calendar-norway-raude-dagar
                calendar-norway-andre-merkedagar
                calendar-norway-dst
                '((holiday-fixed 3 17 "St. Patricksdag") ; extra non-no days
                  (holiday-fixed 10 31 "Hallowe'en")
                  (holiday-float 11 4 4 "Thanksgiving")
                  (solar-equinoxes-solstices)))
        calendar-day-name-array ["Søndag" "Mandag" "Tirsdag" "Onsdag" "Torsdag" "Fredag" "Lørdag"]
        solar-n-hemi-seasons '("Vårjevndøgn" "Sommersolverv" "Høstjegndøgn" "Vintersolherv")))

(t/declare-prefix "oo" "Org"
                  "c" 'org-capture
                  "e" 'org-export-dispatch
                  "g" 'org-mac-grab-link
                  "a" 'org-agenda
                  "n" 'org-alert-check
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
