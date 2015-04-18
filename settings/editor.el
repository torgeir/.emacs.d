;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't truncate lines
(setq truncate-partial-width-windows nil)

;; reflect file system
(global-auto-revert-mode 1)

;; silenced refresh of dired
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; don't use shift for marking
(setq shift-select-mode nil)

;; remove selected text when typing
(delete-selection-mode 1)

;; 80 char lines
(setq fill-column 80)

;; save more recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; undo and redo window config
(winner-mode 1)

;; spaces
(set-default 'indent-tabs-mode nil)
(setq tab-width 2)

;; add dirs to buffer names when not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; more memory
(setq gc-cons-threshold 20000000)

;; go back to last visited place in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; all ~ in backups
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(setq cursor-in-non-selected-windows nil)

;; no scroll
(mouse-wheel-mode t)

;; less jumpy scroll
(setq mouse-wheel-scroll-amount '(2 ((shift) . 2))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboardscroll

(provide 'editor)
