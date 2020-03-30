;;; -*- lexical-binding: t; -*-
(defconst lat-trh 63.427)
(defconst lon-trh 10.391)

(defconst t-user-dropbox-folder (if (or is-mac is-linux)
                                    (t/user-file "Dropbox")
                                  "c:/Users/torgth/Dropbox \(Personlig\)"))

(defun t/user-dropbox-folder (path) (concat t-user-dropbox-folder "/" path))
(defun t/org-directory (path) (concat org-directory "/" path))
(defun t/org-archive-done-tasks ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/DONE" 'file)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/CANCELLED" 'file))

(setq org-directory (t/user-dropbox-folder "org"))
(setq org-mobile-directory (t/user-dropbox-folder "Apps/MobileOrg")
      org-mobile-inbox-for-pull (t/org-directory "inbox.org"))

(comment (defun org-set-local (var val)
           "Seems to have been renamed? Fix missing defun https://lists.gnu.org/archive/html/emacs-orgmode/2016-02/msg00122.html."
           (setq-local var val)))

(setq org-ellipsis " >"
      org-startup-indented t        ; turn on org-indent-mode
      org-return-follows-link t
      org-tab-follows-link nil
      org-hide-leading-stars t
      org-hide-emphasis-markers nil
      org-loop-over-headlines-in-active-region 'start-level ; org-archive with friends work on multiple items
      org-blank-before-new-entry '((heading . auto) (plain-list-item . t)) ; newlines
      org-cycle-separator-lines 2 ; number of empty lines after heading needed to show visible newline between headings
      org-catch-invisible-edits 'show ; show invisibles on edit
      org-enforce-todo-dependencies t ; block parent TODOs if child is not completed
      org-refile-targets '((nil :maxlevel . 2)
                           (org-agenda-files :maxlevel . 2))
      org-refile-use-outline-path 'file ; enable refile to top level in file too
      org-outline-path-complete-in-steps nil ; refile to subpaths
      org-tags-column -60           ; tag position after headings
      org-export-coding-system 'utf-8
      org-default-notes-file (t/org-directory "tasks.org")
      org-special-ctrl-k t         ; don't clear tags, etc
      org-adapt-indentation t      ; move text to align with heading bullets

      ;; doom theme
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-fontify-emphasized-text t

      org-reverse-note-order t      ; newest notes first
      org-log-done 'time            ; log when todos are completed
      org-log-redeadline 'time      ; log when deadline changes
      org-log-reschedule 'time      ; log when schedule changes
      org-use-fast-todo-selection t
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-src-window-setup 'current-window ; edit code src blocks in current window
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil ; don't prompt on every code run
      org-export-babel-evaluate nil ; don't run stuff automatically on export
      org-edit-src-content-indentation 0)

(setq org-html-postamble t
      org-html-postamble-format
      '(("en" "<p class=\"author\">%a (%e)</p>\n<p class=\"date\">%T</p>")))

(defun t/org-capture-chrome-link-template (&optional &rest args)
  (concat "* TODO %? :url:%^G\n\n" (t/grab-chrome-url)))

(defun t/org-capture-elfeed-link-template (&optional &rest args)
  (concat "* TODO %? :url:%^G\n\n%i\n" (elfeed-entry-link elfeed-show-entry)))

(setq org-capture-templates
      `(("t" "Task" entry (file+headline org-default-notes-file "Tasks") "* TODO %? %^G\n\n%i\n\n")
        ("s" "Saga" entry (file+headline ,(t/org-directory "bekk/saga.org") "Saga") "* TODO %? \n\n%i\n\n")
        ("d" "Shared calendar event" entry (file ,(t/org-directory "gcal/delt.org")) "* %?\n")
        ("f" "File location" entry (file+headline org-default-notes-file "Tasks") "* TODO %? %^G\n\n%i%a\n\n")
        ("e" "Elfeed location" entry (file+headline org-default-notes-file "Tasks") (function t/org-capture-elfeed-link-template))
        ("c" "Chrome location" entry (file+headline org-default-notes-file "Tasks") (function t/org-capture-chrome-link-template))))

;; org-mobile
(t/use-package request-deferred :after org)

(t/use-package org
  :ensure org-plus-contrib
  :commands (org-mode)
  :mode ("\\.\\(org\\|org_archive\\)$" . org-mode)
  :init
  (progn
    (t/after org-agenda
      (bind-key "s-s" 'org-save-all-org-buffers org-agenda-mode-map)
      (unbind-key "C-," org-mode-map)) ;; don't need to cycle agenda files

    (evil-add-command-properties #'outline-up-heading :jump t)
    (evil-add-command-properties #'outline-next-heading :jump t)
    (evil-add-command-properties #'outline-previous-heading :jump t)
    (evil-add-command-properties #'org-previous-visible-heading :jump t)
    (evil-add-command-properties #'org-next-visible-heading :jump t)

    (t/declare-prefix "o" "Org"
                      "c" 'org-capture
                      "e" 'org-export-dispatch
                      "g" 'org-mac-grab-link
                      "a" 'org-agenda
                      "A" 't/org-archive-done-tasks
                      "n" 'org-alert-check
                      "hi" 'org-info)

    (t/declare-prefix "om" "Mobile"
                      "p" 'org-mobile-push
                      "P" 'org-mobile-pull)

    (t/declare-prefix "ol" "Links"
                      "s" 'org-store-link
                      "i" 'org-insert-link)

    (t/declare-prefix "ot" "Tags"
                      "a" 'org-archive-set-tag
                      "t" 'org-set-tags-command)

    (t/declare-prefix "oT" "Table"
                      "Tg" 'org-table-toggle-coordinate-overlays
                      "Tf" 'org-table-formula)

    (t/declare-prefix "oC" "Clock"
                      "r" 'org-clock-report
                      "i" 'org-clock-in
                      "o" 'org-clock-out)))

(defun t-org/config ()

  (t/after org

    (progn
      ;; fix completion dissapearing
      (t/after company
        (t/add-company-backends-hook 'org-mode-hook 'company-capf))
      (t/add-hook-defun 'org-mode-hook t/hook-add-pcomplete-to-capf
                        (t/add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t)))

    (progn
      ;; modules
      (setq org-modules '(org-tempo ; templates
                          org-mouse))
      (org-load-modules-maybe t))

    (progn
      ;; misc

      ;;(require 'ox-md)
      (require 'ox-gfm)
      (require 'ob-clojure)

      (setq org-babel-clojure-backend 'cider)

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
         (restclient . t)
         (elasticsearch . t)))

      (t/add-hook 'org-babel-after-execute-hook 't/org-fix-inline-images)

      (t/add-hook-defun 'org-mode-hook t/hook-org
                        ;; bring back stolen smartparen keys
                        (t/bind-in '(evil-motion-state-local-map)
                          "M-<up>" 'org-metaup
                          "M-<down>" 'org-metadown
                          "M-S-<right>" 'org-shiftmetaright
                          "M-S-<left>" 'org-shiftmetaleft)
                        (evil-snipe-override-local-mode)
                        (org-display-inline-images t t)
                        (visual-line-mode 1)))

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


      (defun t/org-agenda-todo-type (name)
        `((org-agenda-remove-tags t)
          (org-agenda-sorting-strategy '(tag-up priority-down))
          (org-agenda-todo-keyword-format "")
          (org-agenda-overriding-header ,name)))


      (defun t/org-agenda-pri-a (&rest tags)
        (string-join (-map (lambda (t) (format "%s&PRIORITY=\"A\"" t)) tags) "|"))

      (defun t/org-agenda-pri (header &rest tags)
        (list (apply 't/org-agenda-pri-a tags)
              `((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header ,header))))
      (defun t/org-agenda-day (tags)
        (list tags '((org-agenda-span 'day)
                     (org-agenda-ndays-to-span 1)
                     (org-agenda-time-grid nil))))

      (defun t/org-agenda-not-pri (header tags skip)
        (list tags `((org-agenda-overriding-header ,header)
                     (org-agenda-skip-function '(or (t/org-skip-subtree-if-priority ?A)
                                                    (org-agenda-skip-if nil (quote ,skip)))))))

      (defun t/org-agenda-todos (header tags)
        (t/org-agenda-not-pri header tags '(scheduled deadline)))

      (defun t/org-agenda-todos-scheduled (header tags)
        (t/org-agenda-not-pri header tags '(notscheduled deadline)))

      (defun t/org-day-summary (&rest tags)
        `((tags ,@(apply 't/org-agenda-pri (append (list "Pri") tags)))
          (agenda ,@(t/org-agenda-day (string-join tags)))
          (tags-todo ,@(t/org-agenda-todos "Todo" (string-join tags)))
          (tags-todo ,@(t/org-agenda-todos-scheduled "Scheduled todo" (string-join tags)))))

      (defun t/org-agenda-read ()
        '(tags-todo "book|read|twitter|pocket" ((org-agenda-overriding-header "Read"))))

      (setq org-agenda-include-diary t
            org-agenda-diary-file (t/org-directory "diary.org")
            org-agenda-default-appointment-duration nil
            org-agenda-window-setup 'current-window;;'only-window ; delete other windows when showing agenda
            org-agenda-restore-windows-after-quit t ; restore them again
            org-agenda-files (t/find-org-files-recursively org-directory "org$\\\|txt$") ; where to look for org files
            org-agenda-text-search-extra-files (t/find-org-files-recursively (t/user-file "Dropbox/org") "org_archive$")
            org-agenda-skip-scheduled-if-done nil ; prevent showing done scheduled items
            org-agenda-custom-commands `(("T" alltodo)
                                         ("C" todo "DONE" ,(t/org-agenda-todo-type "DONE"))
                                         ("t" todo "TODO" ,(t/org-agenda-todo-type "TODO"))
                                         ("b" todo "STARTED" ,(t/org-agenda-todo-type "STARTED"))
                                         ("c" todo "CANCELLED" ,(t/org-agenda-todo-type "CANCELLED"))
                                         ("m" tags-todo "serie|film")
                                         ("e" tags-todo "emacs")
                                         ("r" tags-todo "book|read|twitter|pocket")
                                         ("v" tags-todo "video")
                                         ("w" "bekk" ,(append (t/org-day-summary "+bekk-home")
                                                              `((tags "+someday+bekk"))))
                                         ("s" "saga" ,(append (t/org-day-summary "+saga-home")
                                                              `((tags "+someday+saga"))))
                                         ("d" "datainn" ,(append (t/org-day-summary "+datainn-home")
                                                                 `((tags "+someday+datainn"))))
                                         ("h" "home" ,(append (list (t/org-agenda-read))
                                                              (t/org-day-summary "+home-emacs-someday")
                                                              `((tags-todo "+someday-work" ((org-agenda-overriding-header "Someday"))))))))

      )

    (progn
      ;; realign tags
      (defun t/org-mode-realign-all-tags ()
        "Code to realign tags, stolen from org.el"
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward org-outline-regexp-bol nil t)
            (org-set-tags (org-get-tags nil t))
            (org-set-tags (seq-remove (lambda (tag)
                                        (get-text-property 0 'inherited tag))
                                      (org-get-tags)))
            (end-of-line))))

      (t/add-hook-defun 'before-save-hook t/org-mode-before-save
                        (when (eq major-mode 'org-mode)
                          (t/org-mode-realign-all-tags)))

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
        (defadvice org-metadown (after t/after-org-metadown activate) (t/org-visual-restore))))

    (progn
      ;; save org mode buffers after refile
      (defadvice
          org-refile
          (after t/after-org-refile activate)
        (org-save-all-org-buffers)))

    (progn
      ;; idle timer
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
                      (org-fit-window-to-buffer))
                  (with-selected-window (display-buffer agenda-buffer)
                    (org-fit-window-to-buffer))))
            (call-interactively 'org-agenda-list))))

      (progn

        (defvar t-org-file-save-since-last-idle nil)
        ;; Hook to remember if org files are saved since last idle timer.
        (t/add-hook-defun 'before-save-hook t/org-mode-before-save-since-last-idle
                          (when (eq major-mode 'org-mode)
                            (setq t-org-file-save-since-last-idle t)))

        (defun t/org-idle-timer ()
          "Timer to run when idle for syncing org."
          (interactive)
          (when t-org-file-save-since-last-idle
            (message "Syncing agenda...")
            (org-save-all-org-buffers)
            (comment (t/org-export-calendars))
            (org-mobile-pull)
            (org-mobile-push)
            (setq t-org-file-save-since-last-idle nil)
            (message "Syncing agenda... done")))

        (defun t/org-export-calendars ()
          "Export given set of calendars to ical files, so you can subscribe to their dropbox links in ical.
Locally redefines org-agenda-files not to export all agenda files."
          (interactive)
          (let ((org-agenda-files (cons org-default-notes-file
                                        (mapcar #'t/org-directory
                                                '("home.org"
                                                  "bekk/bekk.org"
                                                  "bekk/saga.org"
                                                  "bekk/datainn.org")))))
            (org-icalendar-export-agenda-files)))

        (when (not is-ms)
          (t/idle-timer t-timers-sync-org-idle #'t/org-idle-timer 5)
          (t/idle-timer t-timers-sync-org-gcal 'org-gcal-fetch 30)))

      (when (boundp 'org-evil-table-mode-map)
        (t/bind-in 'org-evil-table-mode-map
          "M-S-<left>" 'org-table-delete-column
          "M-S-<right>" 'org-table-insert-column))

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
          (if (looking-back "^")
              (call-interactively 'org-meta-return)
            (progn
              (evil-append-line 0)
              (t/call-rebinding-org-blank-behaviour 'org-meta-return))))

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

        (t/add-hook-defun 'org-mode-hook t/hook-org-meta
                          (t/bind-in 'org-mode-map
                            "<return>" 'org-return
                            "C-w" 'org-refile
                            "M-<return>" 't/org-meta-return-dwim
                            "M-S-<return>" 't/org-insert-todo-heading-dwim
                            "C-<return>" 't/org-insert-heading-respect-content-dwim
                            "C-S-<return>" 't/org-insert-todo-heading-respect-content-dwim)))

      (progn
        ;; yas in org
        (defun yas/org-very-safe-expand ()
          (let ((yas/fallback-behavior 'return-nil)) (yas-expand)))

        (defun yas/org-setup ()
          (make-variable-buffer-local 'yas-trigger-key)
          (setq yas-trigger-key [tab])
          (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
          (define-key yas-keymap [tab] 'yas-next-field))

        ;; See https://github.com/eschulte/emacs24-starter-kit/issues/80.
        (setq org-src-tab-acts-natively nil)

        (t/add-hook 'org-mode-hook #'yas/org-setup)))

    (t/add-hook-defun 'org-mode-hook t/reset-org-font-sizes
                      (dolist (face '(org-level-1
                                      org-level-2
                                      org-level-3
                                      org-level-4
                                      org-level-5))
                        (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
    (t/add-hook-defun 'org-mode-hook t/remove-org-mode-stars
                      (set-face-attribute 'org-hide nil :foreground (face-attribute 'default :background)) )))

(t/use-package ob-restclient)

(t/use-package org-alert
  :commands t/org-idle-timer
  :config
  (progn
    (setq alert-default-style 'osx-notifier
          org-alert-interval (* 1 60 60))
    (org-alert-enable)))

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

(t/use-package spray
  :commands spray-mode
  :init
  (progn
    (setq spray-wpm 680
          spray-height 170
          spray-margin-top 0
          spray-margin-left 0)
    (t/declare-prefix "t" "Toggle"
                      "s" (t/micro-state-in-mode
                           'spray-mode
                           "s" 'spray-slower
                           "f" 'spray-faster
                           "SPC" 'spray-start/stop
                           "<left>" 'spray-backward-word
                           "<right>" 'spray-forward-word))
    (t/add-hook-defun 'spray-mode-hook t/hook-spray
                      (setq-local spray-margin-top (truncate (/ (window-height) 2.7)))
                      (setq-local spray-margin-left (truncate (/ (window-width) 2.7)))
                      (beacon-mode -1)
                      (t/locally-disable-cursor)
                      (set-face-foreground 'spray-accent-face
                                           (face-foreground 'font-lock-keyword-face)))))

(t/use-package twittering-mode
  :commands twittering-mode
  :init
  (progn
    (setq twittering-request-confirmation-on-posting t)
    (t/declare-prefix "a" "Applications"
                      "t" 'twittering-mode)))

(t/use-package elfeed
  :commands (elfeed)
  :init
  (progn
    (setq elfeed-db-directory (t/user-file "/Dropbox/Apps/elfeed/db")
          elfeed-search-filter "@6-months-ago -old -gaming -news -life +unread -photo")
    (defun t/elfeed-show-hide-images ()
      (interactive)
      (let ((shr-inhibit-images t))
        (elfeed-show-refresh)))
    (t/declare-prefix "a" "Applications"
                      "r" 'elfeed)
    (t/declare-prefix-for-mode 'elfeed-show-mode
                               "t" "Toggle"
                               "i" 't/elfeed-show-hide-images))
  :config
  (progn
    (evil-set-initial-state 'elfeed-search-mode 'normal)
    (evil-set-initial-state 'elfeed-show-mode 'normal)
    (evil-define-key '(normal motion) elfeed-search-mode-map
      (kbd "<return>") 'elfeed-search-show-entry
      "q" 'quit-window
      "u" 'elfeed-search-tag-all-unread
      "G" 'elfeed-search-fetch
      "r" 'elfeed-search-untag-all-unread
      "s" 'elfeed-search-live-filter ; filter
      "p" 'elfeed-goodies/split-show-prev
      "n" 'elfeed-goodies/split-show-next
      "y" 'elfeed-search-yank)
    (evil-define-key 'normal elfeed-show-mode-map
      "d" 'scroll-up-command
      "u" 'scroll-down-command
      "q" 'elfeed-goodies/delete-pane
      "b" 'elfeed-show-visit)
    (t/add-hook-defun 'elfeed-show-mode-hook t/elfeed-show-mode-hook
                      (let ((map (make-sparse-keymap)))
                        (bind-key "n" (lambda ()
                                        (interactive)
                                        (condition-case nil
                                            (scroll-up-command)
                                          (error (elfeed-goodies/split-show-next)))) map)
                        (bind-key "p" (lambda ()
                                        (interactive)
                                        (condition-case nil
                                            (scroll-down-command)
                                          (error (elfeed-goodies/split-show-prev)))) map)
                        (set-temporary-overlay-map map t
                                                   ;; (lambda () (equal major-mode 'elfeed-show-mode))
                                                   ))
                      (writeroom-mode 1)
                      (visual-line-mode))))

(t/use-package elfeed-goodies
  :commands elfeed-goodies/setup
  :init
  (progn (setq elfeed-goodies/entry-pane-position 'bottom)
         (add-hook 'elfeed-search-mode-hook 'elfeed-goodies/setup)))

(t/use-package elfeed-org
  :commands elfeed-org
  :init
  (progn
    (add-hook 'elfeed-search-mode-hook 'elfeed-org)
    (setq rmh-elfeed-org-files (list "~/Dropbox/org/feeds.org"))))

(t/use-package org-gcal
  :ensure nil
  :load-path "site-lisp/org-gcal/"
  :commands (org-gcal-sync org-gcal-fetch)
  :init
  (progn
    (when (boundp 't-org-gcal)
      (setq org-gcal-client-id t-org-gcal-client-id
            org-gcal-client-secret t-org-gcal-client-secret
            org-gcal-file-alist t-org-gcal-file-alist
            org-gcal-header-alist t-org-gcal-header-alist
            org-gcal-up-days 1)
      (add-hook 'org-agenda-mode-hook 'org-gcal-fetch))))

(t/use-package gnuplot
  :after org)

(t/use-package helm-org-rifle
  :after org
  :commands (helm-org-rifle)
  :init
  (t/declare-prefix-for-mode 'org-mode "s" "Search"
                             "p" 'helm-org-rifle
                             "P" 'helm-projectile-ag))

;; smartparens helpers
(t/after smartparens
  (sp-with-modes 'org-mode
    (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
    (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
    (sp-local-pair "«" "»")))


(provide 't-org)
