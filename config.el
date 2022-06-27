;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Torgeir Thoresen"
      user-mail-address "torgeir.thoresen@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "JetBrains Mono" :size 14))

;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file "~/Dropbox/org/home.org.gpg"
      org-agenda-files '("~/Dropbox/org"))
(setq org-archive-location "%s_archive.gpg::") ; so files are encrypted automatically

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(progn
  (defun t/user-emacs-file (path) (concat user-emacs-directory path))
  (defun t/user-dropbox-folder (path) (expand-file-name (concat "~/Dropbox (Personal)/" path)))
  (defvar is-win IS-WINDOWS)
  (defvar is-cygwin IS-WINDOWS)
  (defvar is-mac IS-MAC)
  (defvar is-linux IS-LINUX)
  (load! "./t-defuns.el"))
;; remove from workspace mode for use with reset-font-size

(setq doom-font-increment 1)

(map!
 :n "s-0" nil
 "s-0" #'doom/reset-font-size
 "s-+" #'doom/increase-font-size
 "s--" #'doom/decrease-font-size

 "s-?" (lambda () (interactive) (text-scale-increase 1))
 "s-_" (lambda () (interactive) (text-scale-decrease 1))
 "s-=" (lambda () (interactive) (text-scale-set 0))

 "s-d" #'t/split-window-right-and-move-there-dammit
 "s-D" #'t/split-window-below-and-move-there-dammit

 "s-M-<up>" 'evil-window-up
 "s-M-<right>" 'evil-window-right
 "s-M-<down>" 'evil-window-down
 "s-M-<left>" 'evil-window-left

 "M-n" 'forward-paragraph
 "M-p" 'backward-paragraph

 "s-k" 'previous-buffer
 "s-j" 'next-buffer
 "s->" 'next-multiframe-window
 "s-<" 'previous-multiframe-window
 "s-<left>" 't/smart-beginning-of-line
 "s-<right>" 'end-of-line


 "C-a" 't/smart-beginning-of-line
 :m "C-e" 'end-of-line

 "s-n" 'make-frame

 ;; op -- :leader :desc "Toggle treemacs" "f L" #'+treemacs/toggle
 ;; oO -- :leader :desc "Open folder" "p o" #'t/open-in-desktop

 :leader :desc "Show calendar" "o c" #'calendar
 :leader :desc "Show home" "o h" #'(lambda () (interactive) (find-file (t/user-dropbox-folder "org/home.org.gpg")))
 :leader :desc "Show saga" "o s" #'(lambda () (interactive) (find-file (t/user-dropbox-folder "org/saga.org.gpg")))
 :leader :desc "Open rss" "o S" #'=rss
 :leader :desc "Eval and replace" "m e R" #'t/eval-and-replace
 :leader :desc "Debug on error" "t D" #'toggle-debug-on-error)

(after! org

  ;; Include gpg files in org agenda
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                    org-agenda-file-regexp)))

  (defun t/org-capture-chrome-link-template (&optional &rest args)
    "Capture current frontmost tab url from chrome."
    (concat "* TODO %? :url:\n\n" (t/grab-chrome-url)))

  (defun t/org-capture-elfeed-link-template (&optional &rest args)
    "Capture open elfeed post with url."
    (concat "* TODO %? :url:%^G\n%i\n" (elfeed-entry-link elfeed-show-entry)))

  (setq org-tags-column -60
        org-support-shift-select t   ; shift can be used to mark multiple lines
        org-special-ctrl-k t         ; don't clear tags, etc
        org-special-ctrl-a/e t       ; don't move past ellipsis on c-e
        org-agenda-skip-scheduled-if-done t
        org-log-done 'time           ; log when todos are completed
        org-log-redeadline 'time     ; log when deadline changes
        org-log-reschedule 'time     ; log when schedule changes
        org-reverse-note-order t     ; newest notes first
        org-return-follows-link t    ; go to http links in browser
        org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "CANCELLED(c)"))
        org-capture-templates
        `(("t" "Task" entry (file+olp org-default-notes-file "Tasks") "* TODO %? \n\n%i\n\n" :prepend t :empty-lines-after 1)
          ("s" "Saga" entry (file+olp ,(t/user-dropbox-folder "org/saga.org.gpg") "Tasks") "* TODO %? \n\n%i" :prepend t :empty-lines-after 1)
          ("b" "bekk" entry (file+olp ,(t/user-dropbox-folder "org/bekk.org.gpg") "Tasks") "* TODO %? \n\n%i" :prepend t :empty-lines-after 1)
          ;;("d" "Shared calendar event" entry (file ,(t/user-dropbox-folder "org/gcal/delt.org.gpg")) "* %?\n" :prepent t)
          ("l" "Link" entry (file+olp org-default-notes-file "Tasks") "* TODO %? %^G\n\nLink: %a" :prepend t :empty-lines-after 1)
          ("f" "File" entry (file+olp org-default-notes-file "Tasks") "* TODO %? %^G\n\n%i%a\n\n" :prepend t :empty-lines-after 1)
          ("c" "Chrome location" entry (file+olp org-default-notes-file "Tasks") (function t/org-capture-chrome-link-template) :prepend t :empty-lines-after 1)
          ("e" "Elfeed location" entry (file+olp org-default-notes-file "Tasks") (function t/org-capture-elfeed-link-template) :prepend t :empty-lines-after 1)

          ;; ("T" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* %?\n %i\n %a" :prepend t)
          ;; ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?\n %i\n %a" :prepend t)
          ;; ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?\n %i\n %a" :prepend t)
          ;; ("p" "Templates for projects") ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n %i\n %a" :prepend t)
          ;; ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n %i\n %a" :prepend t)
          ;; ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n %i\n %a" :prepend t)
          ;; ("o" "Centralized templates for projects")
          ;; ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ;; ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ;; ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)

          )))

;; general
(setq frame-title-format "%b (%f)")

(after! dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; emacs lisp
(after! evil
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode))

;; rss
(after! elfeed
  (setq rmh-elfeed-org-files '("~/Dropbox/org/feeds.org")
        elfeed-search-filter "@2-week-ago +unread"))

;; calendar
(after! calendar
  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize (format "%2d" (car
                                    (calendar-iso-from-absolute
                                     (calendar-absolute-from-gregorian
                                      (list month day year)))))
                     'font-lock-face 'calendar-iso-week-face))
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (copy-face 'default 'calendar-iso-week-header-face))

;; server, make e work from terminal
(setq server-name "torgemacs")

;; time to wait before display
(setq which-key-idle-delay 0.1)

;; macro camelCase to snake_case
(evil-set-register ?c [?: ?s ?/ ?\\ ?\( ?\[ ?a ?- ?z ?0 ?- ?9 ?\] ?\\ ?\) ?\\ ?\( ?\[ ?A ?- ?Z ?0 ?- ?9 ?\] ?\\ ?\) ?/ ?\\ ?1 ?_ ?\\ ?l ?\\ ?2 ?/ ?g])
