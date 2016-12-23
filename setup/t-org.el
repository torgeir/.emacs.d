(defun t/org-todos-by-tag-settings (name)
  `((org-agenda-remove-tags t)
    (org-agenda-sorting-strategy '(tag-up priority-down))
    (org-agenda-todo-keyword-format "")
    (org-agenda-overriding-header ,(concat "\n" name "\n"))))

(defun t/org-day-summary (tags)
  `((tags ,(concat "PRIORITY=\"A\"&" ;; wat lol
                   (replace-regexp-in-string "|" "|PRIORITY=\"A\"&" tags))
          ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
           (org-agenda-overriding-header "Pri")))
    (agenda ,tags
            ((org-agenda-span 'day)
             (org-agenda-ndays 1)
             (org-agenda-time-grid nil)))
    (tags-todo ,tags
               ((org-agenda-overriding-header "Unscheduled")
                (org-agenda-skip-function
                 '(or (t/org-skip-subtree-if-priority ?A)
                      (org-agenda-skip-if nil '(scheduled deadline))))))
    (tags-todo ,tags
               ((org-agenda-overriding-header "Scheduled")
                (org-agenda-skip-function
                 '(or (t/org-skip-subtree-if-priority ?A)
                      (org-agenda-skip-if nil '(notscheduled deadline))))
                (org-show-context-detail 'minimal)
                (org-agenda-view-columns-initially t)))))

(use-package org
  :ensure org-plus-contrib
  :commands (org-mode)
  ;; :bind (:map
  ;;        org-src-mode-map
  ;;        ("C-c C-c" . org-edit-src-exit))
  :bind (:map
         smartparens-mode-map
         ("M-S-<right>" . nil)
         ("M-S-<left>" . nil))
  :init
  (setq user-dropbox-folder (if is-mac "~/Dropbox"
                              "c:/Users/torgth/Dropbox \(Personlig\)"))

  (defun t/user-dropbox-folder (path) (concat user-dropbox-folder "/" path))
  (defun t/org-directory (path) (concat org-directory "/" path))

  (setq org-directory (t/user-dropbox-folder "org"))
  (setq org-mobile-directory (t/user-dropbox-folder "Apps/MobileOrg")
        org-mobile-inbox-for-pull (t/org-directory "inbox.org"))

  (defun org-set-local (var val)
    "Seems to have been renamed? Fix missing defun https://lists.gnu.org/archive/html/emacs-orgmode/2016-02/msg00122.html."
    (setq-local var val))

  (setq org-startup-indented t ; turn on org-indent-mode
        org-return-follows-link t
        org-tab-follows-link nil
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-completion-use-ido t
        org-blank-before-new-entry '((heading . auto) (plain-list-item . t)) ; newlines
        org-cycle-separator-lines 2 ; number of empty lines after heading needed to show visible newline between headings
        org-list-empty-line-terminates-plain-lists t
        org-export-babel-evaluate nil ; don't run stuff automatically on export
        org-confirm-babel-evaluate nil ; don't prompt on every code run
        org-catch-invisible-edits 'show ; show invisibles on edit
        org-enforce-todo-dependencies t ; block parent TODOs if child is not completed
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        org-tags-column -60 ; tag position after headings
        org-export-coding-system 'utf-8
        org-default-notes-file (t/org-directory "tasks.org")
        org-special-ctrl-k t ; don't clear tags, etc
        org-adapt-indentation t ; move text to align with heading bullets

        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t

        org-reverse-note-order t ; newest notes first
        org-log-done 'time ; log when todos are completed
        org-log-redeadline 'time ; log when deadline changes
        org-log-reschedule 'time ; log when schedule changes
        org-use-fast-todo-selection t
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-export-babel-evaluate nil ; don't run stuff automatically on export
        org-edit-src-content-indentation 0)

  (setq org-agenda-include-diary t
        org-agenda-diary-file (t/org-directory "diary.org")
        org-agenda-default-appointment-duration 60
        org-agenda-window-setup 'only-window ; delete other windows when showing agenda
        org-agenda-files (t/find-org-files-recursively org-directory) ; where to look for org files
        org-agenda-skip-scheduled-if-done nil ; prevent showing done scheduled items
        org-agenda-custom-commands `(("w" . "Work")
                                     ("ww" "bekk" ,(t/org-day-summary "+bekk") ((org-agenda-remove-tags t)))
                                     ("wd" "datainn" ,(t/org-day-summary "+bekk-sb1|+datainn-sb1") ((org-agenda-remove-tags t)))
                                     ("ws" "sb1" ,(t/org-day-summary  "+bekk-datainn|+sb1-datainn") ((org-agenda-remove-tags t)))

                                     ("t" . "Todos")
                                     ("ta" alltodo)
                                     ("tt" todo "TODO" ,(t/org-todos-by-tag-settings "TODO tasks by tag"))
                                     ("ts" todo "STARTED" ,(t/org-todos-by-tag-settings "STARTED tasks by tag"))
                                     ("tc" todo "CANCELLED" ,(t/org-todos-by-tag-settings "CANCELLED tasks by tag"))
                                     ("td" todo "DONE" ,(t/org-todos-by-tag-settings "DONE tasks by tag"))

                                     ("h" . "Home")
                                     ("hs" tags-todo "serie")
                                     ("he" tags-todo "emacs")
                                     ("hb" tags-todo "book")
                                     ("hv" tags-todo "video")

                                     ("d" "Deadlines" agenda ""
                                      ((org-agenda-entry-types '(:deadline))
                                       (org-agenda-ndays 1)
                                       (org-deadline-warning-days 60)
                                       (org-agenda-time-grid nil)))))
  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<p class=\"author\">%a (%e)</p>\n<p class=\"date\">%T</p>")))

  (setq org-capture-templates
        `(("t" "Task"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %? %^G\nSCHEDULED: %t\n%i\n%a")

          ("j" "Journal entry"
           entry
           (file+datetree ,(t/org-directory "journal.org"))
           "**** %U %^{Title}\n%?")))

  :config
  (setq org-modules '(org-mouse
                      ;; TODO error when loading these two
                      ;;org-eval
                      ;;org-expiry
                      ))

  (eval-after-load 'org
    '(org-load-modules-maybe t))

  (eval-after-load 'org
    '(require 'ox-md))

  (use-package ob-restclient
    :after org)

  (with-eval-after-load 'org
    (require 'ob-clojure)
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
       (restclient . t))))

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
              (bind-key [tab] 'yas-next-field-or-maybe-expand yas/keymap)))


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

(defun t/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun t/jump-to-org-agenda ()
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
      (call-interactively 'org-agenda-list))))

(run-with-idle-timer 300 t #'t/jump-to-org-agenda)

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

;; (use-package org-bullets
;;   :after org
;;   :commands org-bullets-mode
;;   :init
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; TODO ox-icalendar?
(setq org-icalendar-use-scheduled '(event-if-todo))

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
        solar-n-hemi-seasons '("Vårjevndøgn" "Sommersolverv" "Høstjevndøgn" "Vintersolherv")))

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
                  "t" 'org-set-tags-command)

(t/declare-prefix "ooT" "Table"
                  "Tg" 'org-table-toggle-coordinate-overlays
                  "Tf" 'org-table-formula)

(t/declare-prefix "ooC" "Clock"
                  "i" 'org-clock-in
                  "o" 'org-clock-out)

(progn
  ;; blank line before new entries with text,
  ;; but not headings following other headings (todolists)

  (setq org-blank-before-new-entry
        '((heading . always)
          (plain-list-item . nil)))

  (defun t/call-rebinding-org-blank-behaviour (fn)
    (let ((org-blank-before-new-entry
           (copy-tree org-blank-before-new-entry)))
      (when (org-at-heading-p)
        (rplacd (assoc 'heading org-blank-before-new-entry) nil))
      (call-interactively fn)))

  (defun t/org-meta-return-dwim ()
    (interactive)
    (t/call-rebinding-org-blank-behaviour 'org-meta-return))

  (defun t/org-insert-todo-heading-dwim ()
    (interactive)
    (t/call-rebinding-org-blank-behaviour 'org-insert-todo-heading))

  (defun t/org-insert-heading-respent-content-dwim ()
    (interactive)
    (t/call-rebinding-org-blank-behaviour 'org-insert-heading-respect-content))

  (defun t/org-insert-todo-heading-respect-content-dwim ()
    (interactive)
    (t/call-rebinding-org-blank-behaviour 'org-insert-todo-heading-respect-content))

  (add-hook 'org-mode-hook
            (lambda ()
              (bind-key "M-<return>" 't/org-meta-return-dwim org-mode-map)
              (bind-key "M-S-<return>" 't/org-insert-todo-heading-dwim org-mode-map)
              (bind-key "C-<return>" 't/org-insert-heading-respent-content-dwim org-mode-map)
              (bind-key "C-S-<return>" 't/org-insert-todo-heading-respect-content-dwim org-mode-map))))

(provide 't-org)
