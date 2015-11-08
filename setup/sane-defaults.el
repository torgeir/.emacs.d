(setq initial-frame-alist '((top . 25) (left . 80) (width . 140) (height . 38)))

;; paste copies to os
(setq interprogram-cut-function #'copy-to-clipboard)

;; update screen immediately
(setq redisplay-dont-pause t)

;; dont blink cursor
(blink-cursor-mode -1)

;; no bell
(setq ring-bell-function 'ignore)

;; visible bell
(setq visible-bell t)

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

;; no splash
(setq inhibit-startup-message t)

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

;; reflect file system
(global-auto-revert-mode 1)

;; silenced refresh of dired
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; remove selected text when typing
(delete-selection-mode 1)

;; 80 char lines
(setq fill-column 80)

;; save more recent files
(recentf-mode 1)
(setq recentf-max-saved-items 50)

;; undo and redo window config
(winner-mode 1)

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

;; no cursor in other open windows
(setq cursor-in-non-selected-windows nil)

(when (fboundp 'winner-mode)
  ;; c-c <left> goes to previous window configuration
  (winner-mode 1))

;; add dirs to buffer names when not unique
(setq uniquify-buffer-name-style 'forward)

;; mouse, with scroll
(xterm-mouse-mode t)
(defun trackp-mouse (e))
(setq mouse-sel-mode t)
(when (require 'mwheel nil 'noerror)
  (bind-key [mouse-4] '(lambda () (interactive) (scroll-down 1)))
  (bind-key [mouse-5] '(lambda () (interactive) (scroll-up 1)))
  (mouse-wheel-mode t))

(use-package smooth-scrolling)

;; utf-8 ffs
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; one space between sentences
(setq sentence-end-double-space nil)

;; Enforce proper whitespace
(use-package ethan-wspace
  :init
  (setq mode-require-final-newline nil)
  (setq require-final-newline nil)
  :config
  (global-ethan-wspace-mode 1))

(provide 'sane-defaults)
