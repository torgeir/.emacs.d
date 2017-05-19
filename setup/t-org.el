(defconst lat-trh 63.427)
(defconst lon-trh 10.391)

(defun t-org/funcs ()
  (defconst t-user-dropbox-folder (if is-mac
                                      (t/user-file "/Dropbox")
                                    "c:/Users/torgth/Dropbox \(Personlig\)"))

  (defun t/user-dropbox-folder (path) (concat t-user-dropbox-folder "/" path))
  (defun t/org-directory (path) (concat org-directory "/" path)))

(defun t-org/vars ()
  (setq org-directory (t/user-dropbox-folder "org"))
  (setq org-mobile-directory (t/user-dropbox-folder "Apps/MobileOrg")
        org-mobile-inbox-for-pull (t/org-directory "inbox.org"))

  (defun org-set-local (var val)
    "Seems to have been renamed? Fix missing defun https://lists.gnu.org/archive/html/emacs-orgmode/2016-02/msg00122.html."
    (setq-local var val))

  (setq org-startup-indented t        ; turn on org-indent-mode
        org-agenda-window-setup 'only-window ; remove other windows when agenda
        org-agenda-restore-windows-after-quit t ; restore them again
        org-return-follows-link t
        org-tab-follows-link nil
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-loop-over-headlines-in-active-region 'start-level ; org-archive with friends work on multiple items
        org-completion-use-ido t
        org-blank-before-new-entry '((heading . auto) (plain-list-item . t)) ; newlines
        org-cycle-separator-lines 2 ; number of empty lines after heading needed to show visible newline between headings
        org-list-empty-line-terminates-plain-lists t
        org-catch-invisible-edits 'show ; show invisibles on edit
        org-enforce-todo-dependencies t ; block parent TODOs if child is not completed
        org-refile-targets '((nil :maxlevel . 2)
                             (org-agenda-files :maxlevel . 2))
        org-tags-column -60           ; tag position after headings
        org-export-coding-system 'utf-8
        org-default-notes-file (t/org-directory "tasks.org")
        org-special-ctrl-k t         ; don't clear tags, etc
        org-adapt-indentation t      ; move text to align with heading bullets

        ;; doom theme
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t

        org-reverse-note-order t      ; newest notes first
        org-log-done 'time            ; log when todos are completed
        org-log-redeadline 'time      ; log when deadline changes
        org-log-reschedule 'time      ; log when schedule changes
        org-use-fast-todo-selection t
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)")))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil ; don't prompt on every code run
        org-export-babel-evaluate nil ; don't run stuff automatically on export
        org-edit-src-content-indentation 0)

  (setq org-html-postamble t
        org-html-postamble-format
        '(("en" "<p class=\"author\">%a (%e)</p>\n<p class=\"date\">%T</p>")))

  (setq org-capture-templates
        `(("t" "Task"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %? %^G\n%i")

          ("j" "Journal entry"
           entry
           (file+datetree ,(t/org-directory "journal.org"))
           "**** %U %^{Title}\n%?")))
  )

(t/use-package org
  :ensure org-plus-contrib
  :commands (org-mode)
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode))

(defun t-org/config ()

  (with-eval-after-load 'org

    (progn
      ;; fix completion dissapearing

      (with-eval-after-load 'company (add-to-list 'company-backends 'company-capf))
      (defun add-pcomplete-to-capf () (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
      (add-hook 'org-mode-hook #'add-pcomplete-to-capf))

    (progn
      ;; modules
      (setq org-modules '(org-mouse))
      (org-load-modules-maybe t)))

  (progn
    ;; misc

    ;;(require 'ox-md)
    (require 'ob-clojure)

    (setq org-babel-clojure-backend 'cider) ; use cider instead of slime (default)

    (defconst org-babel-clojure-nrepl-timeout 20)
    (defun org-babel-execute:clojure (body params)
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
            (error result)))))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (gnuplot . t)
       (clojure . t)
       (python . t)
       (ruby . t)
       (js . t)
       (latex . t)
       (shell . t)
       (dot . t)
       (restclient . t)))

    (add-hook 'org-babel-after-execute-hook 't/org-fix-inline-images)

    (add-hook 'org-mode-hook
              (lambda ()
                (org-display-inline-images t t)
                (visual-line-mode 1)    ; wrap long lines
                ;; (progn
                ;;   ;; yasnippet
                ;;   (make-variable-buffer-local 'yas/trigger-key)
                ;;   (org-set-local 'yas/trigger-key [tab])
                ;;   (bind-key [tab] 'yas-next-field-or-maybe-expand yas/keymap))
                )))

  (progn
    ;; agenda

    (defun t/org-skip-subtree-if-priority (priority)
      "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (pri-value (* 1000 (- org-lowest-priority priority)))
            (pri-current (org-get-priority (thing-at-point 'line t))))
        (if (= pri-value pri-current)
            subtree-end
          nil)))


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

    (setq org-agenda-include-diary t
          org-agenda-diary-file (t/org-directory "diary.org")
          org-agenda-default-appointment-duration nil
          org-agenda-window-setup 'only-window ; delete other windows when showing agenda
          org-agenda-files (t/find-org-files-recursively org-directory "org") ; where to look for org files
          org-agenda-text-search-extra-files (t/find-org-files-recursively (t/user-file "/Dropbox/org") "org_archive")
          org-agenda-skip-scheduled-if-done nil ; prevent showing done scheduled items
          org-agenda-custom-commands `(("w" . "Work")
                                       ("wh" "home" ,(t/org-day-summary "+home") ((org-agenda-remove-tags t)))
                                       ("ww" "bekk" ,(t/org-day-summary "+bekk") ((org-agenda-remove-tags t)))
                                       ("wd" "datainn" ,(t/org-day-summary "+datainn") ((org-agenda-remove-tags t)))

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
                                         (org-agenda-time-grid nil))))))

  (progn
    ;; realign tags

    (defun t/org-mode-before-save ()
      "Hook run before saving org buffers"
      (when (eq major-mode 'org-mode)
        (t/org-mode-realign-all-tags)))

    (defun t/org-mode-realign-all-tags ()
      "Code to realign tags, stolen from org.el"
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-outline-regexp-bol nil t)
          (org-set-tags nil t)
          (end-of-line))))

    (add-hook 'before-save-hook #'t/org-mode-before-save))

  (progn
    ;; reselect visual when moving multiple lines
    (setq t-org-move-tree-was-visual nil)
    (defun t/org-visual-restore ()
      (when t-org-move-tree-was-visual
        (evil-normal-state)
        (evil-visual-restore)
        (setq t-org-move-tree-was-visual nil)))
    (defadvice org-metaup   (before t/before-org-metaup activate) (setq t-org-move-tree-was-visual (region-active-p)))
    (defadvice org-metadown (before t/before-org-metadown activate) (setq t-org-move-tree-was-visual (region-active-p)))
    (defadvice org-metaup   (after t/after-org-metaup activate) (t/org-visual-restore))
    (defadvice org-metadown (after t/after-org-metadown activate) (t/org-visual-restore)))

  (progn
    ;; save org mode buffers after refile
    (defadvice
        org-refile
        (after t/after-org-refile activate)
      (org-save-all-org-buffers)))

  (progn
    ;; idle timer

    (when (boundp 'spacemacs-useful-buffers-regexp)
      (add-to-list 'spacemacs-useful-buffers-regexp "\\*Org Agenda\\*"))

    (defun t/jump-to-org-agenda ()
      (interactive)
      (let ((agenda-buffer (get-buffer "*Org Agenda*"))
            wind)
        (if (and (not (equal agenda-buffer (current-buffer)))
                 agenda-buffer)
            (if (setq wind (get-buffer-window agenda-buffer))
                (select-window wind)
              (if (called-interactively-p)
                  (progn
                    (select-window (display-buffer agenda-buffer t t))
                    (org-fit-window-to-buffer)
                    (org-agenda-redo t))
                (with-selected-window (display-buffer agenda-buffer)
                  (org-fit-window-to-buffer)
                  (org-agenda-redo t))))
          (call-interactively 'org-agenda-list))))

    (progn

      (defvar t-org-file-save-since-last-idle nil)

      (defun t/org-mode-before-save-since-last-idle ()
        "Hook to remember if org files are saved since last idle timer."
        (when (eq major-mode 'org-mode)
          (setq t-org-file-save-since-last-idle t)))

      (add-hook 'before-save-hook #'t/org-mode-before-save-since-last-idle)

      (defun t/org-idle-timer ()
        "Timer to run when idle for syncing org."
        (interactive)
        (when t-org-file-save-since-last-idle
          (message "Syncing agenda...")
          (org-save-all-org-buffers)
          (t/org-export-calendars)
          (org-mobile-pull)
          (org-mobile-push)
          (setq t-org-file-save-since-last-idle nil)
          (message "Syncing agenda... done"))
        (t/jump-to-org-agenda))

      (defun t/org-export-calendars ()
        "Export given set of calendars to ical files, so you can subscribe to their dropbox links in ical.
Locally redefines org-agenda-files not to export all agenda files."
        (interactive)
        (let ((org-agenda-files (cons org-default-notes-file
                                      (mapcar #'t/org-directory
                                              '("home.org"
                                                "bekk/bekk.org"
                                                "bekk/datainn.org")))))
          (org-icalendar-export-agenda-files)))

      (t/idle-timer t-timers-sync-org-idle #'t/org-idle-timer 5)
      (t/idle-timer t-timers-sync-org-gcal 'org-gcal-sync 60))

    (progn
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

    (when (boundp 'org-evil-table-mode-map)
      (bind-key "M-S-<left>" 'org-table-delete-column org-evil-table-mode-map)
      (bind-key "M-S-<right>" 'org-table-insert-column org-evil-table-mode-map))

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
        (evil-append-line 0)
        (t/call-rebinding-org-blank-behaviour 'org-meta-return))

      (defun t/org-insert-todo-heading-dwim ()
        (interactive)
        (t/call-rebinding-org-blank-behaviour 'org-insert-todo-heading)
        (evil-cp-append 1))

      (defun t/org-insert-heading-respect-content-dwim ()
        (interactive)
        (t/call-rebinding-org-blank-behaviour 'org-insert-heading-respect-content)
        (evil-cp-append 1))

      (defun t/org-insert-todo-heading-respect-content-dwim ()
        (interactive)
        (t/call-rebinding-org-blank-behaviour 'org-insert-todo-heading-respect-content)
        (evil-cp-append 1))

      (add-hook 'org-mode-hook
                (lambda ()
                  (bind-key "C-w" 'org-refile org-mode-map)
                  (bind-key "M-<return>" 't/org-meta-return-dwim org-mode-map)
                  (bind-key "M-S-<return>" 't/org-insert-todo-heading-dwim org-mode-map)
                  (bind-key "C-<return>" 't/org-insert-heading-respect-content-dwim org-mode-map)
                  (bind-key "C-S-<return>" 't/org-insert-todo-heading-respect-content-dwim org-mode-map))))

    (defun yas/org-very-safe-expand ()
      (let ((yas/fallback-behavior 'return-nil)) (yas-expand)))

    (defun yas/org-setup ()
      ;; yasnippet (using the new org-cycle hooks)
      (yas-global-mode 1)
      (make-variable-buffer-local 'yas-trigger-key)
      (setq yas-trigger-key [tab])
      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
      (define-key yas-keymap [tab] 'yas-next-field))

    ;; See https://github.com/eschulte/emacs24-starter-kit/issues/80.
    (setq org-src-tab-acts-natively nil)

    (add-hook 'org-mode-hook #'yas/org-setup)))

(t/use-package ob-restclient
  :after org)

(t/use-package org-alert
  :config
  (progn
    (setq alert-default-style 'osx-notifier
          org-alert-interval (* 1 60 60))
    (org-alert-enable)))

(t/use-package org-mac-iCal
  :after org
  :commands (org-mac-iCal))

(t/use-package weather-metno
  :after org
  :config
  (progn
    (setq weather-metno-location-name "Trondheim, Norway"
          weather-metno-location-latitude lat-trh
          weather-metno-location-longitude lon-trh
          ;; Emacs 25 doesn't play nice with image magick 7?
          ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25967
          ;; weather-metno-use-imagemagick t
          ;; weather-metno-get-image-props '(:width 20 :height 20 :ascent center)
          )))

(t/use-package org-mac-link
  :after org-plus-contrib
  :ensure org-plus-contrib
  :commands (org-mac-grab-link
             org-mac-chrome-get-frontmost-url))

(t/use-package calendar-norway
  :after calendar
  :config
  (progn
    (setq calendar-holidays
          (append calendar-norway-raude-dagar
                  calendar-norway-andre-merkedagar
                  calendar-norway-dst
                  '((holiday-fixed 3 17 "St. Patricksdag") ; extra non-no days
                    (holiday-fixed 10 31 "Hallowe'en")
                    (holiday-float 11 4 4 "Thanksgiving")
                    (solar-equinoxes-solstices)))
          calendar-day-name-array ["Søndag" "Mandag" "Tirsdag" "Onsdag" "Torsdag" "Fredag" "Lørdag"]
          solar-n-hemi-seasons '("Vårjevndøgn" "Sommersolverv" "Høstjevndøgn" "Vintersolherv"))

    (setq calendar-latitude lat-trh
          calendar-longitude lon-trh
          calendar-location-name "Trondheim, Norway")

    (progn
      ;; moons in agenda
      (with-no-warnings (defvar date))
      (defun t/org-lunar-phases ()
        "Show lunar phase in Agenda buffer."
        (require 'lunar)
        (let* ((phase-list (lunar-phase-list (nth 0 date) (nth 2 date)))
               (phase (cl-find-if (lambda (phase) (equal (car phase) date))
                                  phase-list)))
          (when phase
            (setq ret (concat (lunar-phase-name (nth 2 phase)) " "
                              (substring (nth 1 phase) 0 5))))))

      (defadvice lunar-phase-name (around sv-lunar-phase-name activate)
        "Månefasenavn på norsk."
        (setq ad-return-value
              (let ((phase (ad-get-arg 0)))
                (cond ((= 0 phase) "Nymåne ●")
                      ((= 1 phase) "Månen i ny ☽")
                      ((= 2 phase) "Fullmåne ○")
                      ((= 3 phase) "Månen i ne ☾"))))))))

(t/use-package elfeed
  :commands (elfeed-search-mode elfeed-show-mode)
  :init
  (progn
    (setq elfeed-db-directory (t/user-file "/Dropbox/Apps/elfeed/db")
          elfeed-search-filter "@6-months-ago +unread -old"
          shr-use-fonts nil
          shr-max-image-proportion 0.3)

    (with-eval-after-load 'evil
      (progn
        (add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
        (add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)))

    (t/declare-prefix "o" "Other"
                      "r" 'elfeed))
  :config
  (progn
    (bind-key "SPC" 'elfeed-search-show-entry elfeed-search-mode-map)))

(t/use-package elfeed-goodies
  :after elfeed
  :config
  (progn
    (elfeed-goodies/setup)))

(t/use-package elfeed-org
  :after elfeed
  :config
  (progn
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Dropbox/org/feeds.org"))))

(t/use-package org-gcal
  :init
  (progn
    (when (boundp 't-org-gcal)
      (setq org-gcal-client-id t-org-gcal-client-id
            org-gcal-client-secret t-org-gcal-client-secret
            org-gcal-file-alist t-org-gcal-file-alist))))

(t/use-package gnuplot)

(t/use-package helm-org-rifle
  :config
  (t/declare-prefix-for-mode 'org-mode "s" "Search"
                             "p" 'helm-org-rifle
                             "P" 'helm-projectile-ag))

;; smartparens helpers
(with-eval-after-load 'smartparens
(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»")))


(provide 't-org)
