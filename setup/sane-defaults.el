;; cut copies to os
(setq interprogram-cut-function #'t/copy-to-clipboard)

;; update screen immediately
(setq redisplay-dont-pause t)

;; dont blink cursor
(blink-cursor-mode -1)

;; draw underline lower
(setq x-underline-at-descent-line t)

;; no bell
(setq ring-bell-function 'ignore)

;; visible bell
(setq visible-bell t)

;; focus help buffers
(setq help-window-select 't)

;; start *scratch* in text mode, loads emacs faster
(setq initial-major-mode 'text-mode)

;; clipboard contents into kill-ring before replace
(setq save-interprogram-paste-before-kill t)

;; show keystrokes
(setq echo-keystrokes 0.01)

;; gaudiest possible look
(setq font-lock-maximum-decoration t)

;; show active region
(transient-mark-mode 0)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; show parens
(show-paren-mode t)
(setq show-paren-delay 0)

;; no splash
(setq inhibit-startup-message t)

;; remove menu
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

;; show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; full path in titlebar
(setq-default frame-title-format "%b (%f)")

;; highlight TODOs
(defface todo-face '((t (:foreground "white" :background "green" :bold t)))
  "Face used for highlighting todos" :group 'basic-faces)
(add-hook 'prog-mode-hook (lambda () (font-lock-add-keywords nil '(("\\(TODO\\)" 1 'todo-face t)))))

;; show trailing whitespace in red
(--each '(emacs-lisp-mode-hook
          clojure-mode-hook
          js2-mode-hook)
  (add-hook it (lambda () (setq show-trailing-whitespace t))))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't truncate lines
(setq truncate-partial-width-windows nil)

;; remove selected text when typing
(delete-selection-mode t)

;; 80 char lines
(setq fill-column 80)

;; above what sizes can the window split
(setq split-height-threshold 40
      split-width-threshold 40)

;; spaces
(setq tab-width 2)

;; more memory
(setq gc-cons-threshold 20000000)

;; temp files in..
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file ".backups/"))))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file ".auto-save-list/") t)))
(setq auto-save-list-file-prefix (locate-user-emacs-file ".auto-save-list/"))
(setq recentf-save-file (locate-user-emacs-file ".recentf"))
(setq save-place-file (locate-user-emacs-file ".places"))
(setq save-place-forget-unreadable-files nil)
(setq create-lockfiles nil)
(setq ido-save-directory-list-file (locate-user-emacs-file ".ido.last"))

;; save more recent files
(use-package recentf
  :defer t
  :init
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))
  :config
  (recentf-mode 1))

;; no cursor in other open windows
(setq cursor-in-non-selected-windows nil)

(use-package winner
  :ensure nil
  :config (winner-mode 1))

;; add dirs to buffer names when not unique
(use-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'forward))

;; mouse, with scroll
(xterm-mouse-mode t)
(defun trackp-mouse (e))
(setq mouse-sel-mode t)
(when (require 'mwheel nil 'noerror)
  (bind-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
  (bind-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
  (mouse-wheel-mode t))

(use-package smooth-scrolling
  :defer 1
  :init
  (setq smooth-scroll-margin 10)
  :config
  (smooth-scrolling-mode)
  (enable-smooth-scroll-for-function previous-line)
  (enable-smooth-scroll-for-function next-line)
  (enable-smooth-scroll-for-function isearch-repeat))

;; utf-8 ffs
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(add-to-list 'file-coding-system-alist '("\\.org" . utf-8))
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "" 'utf-8)

;; one space between sentences
(setq sentence-end-double-space nil)

;; silence useless warnings, e.g. ad-handle-definition: `find-tag-noselect' got redefined
(setq ad-redefinition-action 'accept)

(provide 'sane-defaults)
