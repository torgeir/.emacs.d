(use-package dash :config (dash-enable-font-lock)); list helpers
(use-package s) ; string helpers
(use-package f) ; file helpers
(use-package all-the-icons) ; pretty icons
(use-package better-defaults) ; rid the insanity

;; utf-8 ffs
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(add-to-list 'file-coding-system-alist '("\\.org" . utf-8))
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8)

(setq-default
 redisplay-dont-pause t ; update screen immediately
 x-underline-at-descent-line t ; draw underline lower
 help-window-select 't ; focus help buffers
 visible-bell t ; visible bell
 ring-bell-function 'ignore ; no bell
 compilation-scroll-output 'first-error ; scroll compilation to first error
 window-combination-resize t ; resize proportionally
 initial-major-mode 'text-mode ; load *scratch* in text-mode
 initial-scratch-message nil ; clear *scratch* buffer
 echo-keystrokes 0.001 ; show keystrokes
 save-interprogram-paste-before-kill t ; clipboard contents into kill-ring before replace
 font-lock-maximum-decoration t ; gaudiest possible look
 truncate-partial-width-windows nil ; don't truncate lines
 indicate-empty-lines nil ; don't show empty lines after buffer
 indicate-buffer-boundaries nil ; don't show buffer start/end
 fringes-outside-margins t       ; switches order of fringe and margin
 frame-title-format "%b (%f)"; full path in titlebar
 inhibit-startup-message t ; no splash
 sentence-end-double-space nil ; one space between sentences
 ad-redefinition-action 'accept ; silence useless warnings, e.g. ad-handle-definition: `find-tag-noselect' got redefined
 fill-column 80 ; chars per line
 gc-cons-threshold 20000000 ; more memory
 indent-tabs-mode nil ; don't use tabs
 tab-width 2 ; two spaces
 cursor-in-non-selected-windows nil ; no cursor in other open windows
 eval-expression-print-length nil ; no length limit when printing sexps in message buffer
 eval-expression-print-level nil) ; no level limit when printing sexps in message buffer

;; y or n will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; dont blink cursor
(blink-cursor-mode -1)

;; remove menus
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;; show active region
(transient-mark-mode 0)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; show parens
(show-paren-mode t)
(setq show-paren-delay 0)

;; remove selected text when typing
(delete-selection-mode t)

;; mouse, with scroll
(xterm-mouse-mode t)
(defun trackp-mouse (e))
(setq mouse-sel-mode t)
(when (require 'mwheel nil 'noerror)
  (bind-key [wheel-down] '(lambda () (interactive) (scroll-down 2)))
  (bind-key [wheel-up] '(lambda () (interactive) (scroll-up 2)))
  (bind-key [mouse-4] '(lambda () (interactive) (scroll-down 2)))
  (bind-key [mouse-5] '(lambda () (interactive) (scroll-up 2)))
  (mouse-wheel-mode t))

;; above what sizes can the window split
(setq split-height-threshold 0
      split-width-threshold 0
      split-window-preferred-function #'t/split-window-sensibly)

;; temp files in..
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backups/")))
      auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file ".auto-save-list/") t))
      auto-save-list-file-prefix (locate-user-emacs-file ".auto-save-list/")
      recentf-save-file (locate-user-emacs-file ".recentf")
      save-place-file (locate-user-emacs-file ".places")
      save-place-forget-unreadable-files nil
      create-lockfiles nil
      ido-save-directory-list-file (locate-user-emacs-file ".ido.last"))

(provide 't-sane-defaults)
