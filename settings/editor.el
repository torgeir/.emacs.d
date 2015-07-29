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

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq cursor-in-non-selected-windows nil)

;; mouse, with scroll
(require 'mouse)
(xterm-mouse-mode t)
(defun trackp-mouse (e))
(setq mouse-sel-mode t)
(when (require 'mwheel nil 'noerror)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  (mouse-wheel-mode t))

(setq company-idle-delay 0.2)
(add-hook 'after-init-hook 'global-company-mode)

(global-git-gutter+-mode t)

(guru-global-mode +1)

(require 'projectile)
(projectile-global-mode)

;; don't show highlights
(transient-mark-mode 0)

;; don't force newline at end of files on visit
(setq-default require-final-newline nil)
(setq-default mode-require-final-newline nil)

(require 'neotree)
(setq neo-window-width 30)
(setq neo-theme 'nerd)
(setq neo-smart-open nil)

(require 'undo-tree)

(provide 'editor)