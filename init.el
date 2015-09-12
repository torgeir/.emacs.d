(setq dir-snippets (locate-user-emacs-file "snippets"))
(setq dir-setup (locate-user-emacs-file "setup"))
(setq dir-site-lisp (locate-user-emacs-file "site-lisp"))

(add-to-list 'load-path dir-setup)
(add-to-list 'load-path dir-site-lisp)

;; add folders inside site-lisp as well
(dolist (project (directory-files dir-site-lisp t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; don't load outdated bytecode
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  ;; fetch the ones missing
  (setq use-package-always-ensure t))
(require 'diminish)
(require 'bind-key)

(use-package benchmark-init
  :ensure nil)

;; packages

(setq is-mac (equal system-type 'darwin))
(setq is-cygwin (equal system-type 'cygwin))
(setq is-linux (equal system-type 'gnu/linux))
(setq is-win (equal system-type 'windows-nt))

(use-package dash)
(use-package s)
(use-package better-defaults)

(require 'sane-defaults)
(require 'defuns)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package rainbow-mode
  :config
  (dolist (mode-hook '(prog-mode-hook css-mode-hook html-mode-hook))
    (add-hook mode-hook #'rainbow-mode)))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

(use-package tramp
  :ensure nil
  :defer t
  :config
  ;; store auto-save files locally
  (setq tramp-auto-save-directory (locate-user-emacs-file ".tramp-auto-save")))

(use-package paredit
  :diminish paredit-mode
  :config
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       eval-expression-minibuffer-setup-hook
                       ielm-mode-hook
                       lisp-mode-hook
                       lisp-interaction-mode-hook
                       scheme-mode-hook))
    (add-hook mode-hook #'enable-paredit-mode))
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       lisp-interaction-mode-hook
                       ielm-mode-hook))
    (add-hook 'mode-hook 'turn-on-eldoc-mode)))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  ;; percentage height
  (setq which-key-side-window-max-height 0.5)

  ;; time to wait before display
  (setq which-key-idle-delay 0.8)
  (which-key-mode 0)
  (which-key-mode))

(use-package windmove
  :ensure nil
  :bind (("C-c w b" . windmove-left)
         ("C-c w f" . windmove-right)
         ("C-c w p" . windmove-up)
         ("C-c w n" . windmove-down)))

(use-package subword
  :defer t
  :ensure nil
  :config (subword-mode))

(use-package projectile
  :init
  (setq projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld"))
  :config (projectile-global-mode))

(use-package neotree
  :commands neotree
  :bind (("C-x n f" . neotree-find)
         ([f6] . neotree-toggle))
  :init
  (setq neo-window-width 30)
  (setq neo-smart-open nil)
  (when is-mac (setq neo-theme 'nerd))
  :config
  (add-hook 'neo-enter-hook
            '(lambda (one two three)
               (define-key neotree-mode-map (kbd "M-n") 'neotree-change-root)
               (define-key neotree-mode-map (kbd "M-p") (lambda ()
                                                          (interactive)
                                                          (neotree-select-up-node)
                                                          (neotree-select-up-node))))))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (setq git-gutter+-modified-sign "  ")
  (setq git-gutter+-added-sign "+ ")
  (setq git-gutter+-deleted-sign "- ")
  (global-git-gutter+-mode t)
  (bind-key "C-x v n" 'git-gutter+-next-hunk git-gutter+-mode-map)
  (bind-key "C-x v p" 'git-gutter+-previous-hunk git-gutter+-mode-map)
  (bind-key "C-x v C" 'git-gutter+-stage-and-commit git-gutter+-mode-map)
  (bind-key "C-x v =" 'git-gutter+-show-hunk git-gutter+-mode-map)
  (bind-key "C-x v r" 'git-gutter+-revert-hunks git-gutter+-mode-map)
  (bind-key "C-x v s" 'git-gutter+-stage-hunks git-gutter+-mode-map)
  (bind-key "C-x v c" 'git-gutter+-commit git-gutter+-mode-map))

(use-package git-timemachine
  :commands git-timemachine
  :defer t
  :bind ("C-x v t" . git-timemachine-toggle))

(use-package gist
  :commands gist-list
  :bind ("C-x g" . gist-list))

(use-package magit
  :commands magit-status
  :defer t
  :bind ("C-x m" . magit-status)
  :config
  (bind-key "q" #'magit-quit-session magit-status-mode-map)
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)
    (git-gutter+-refresh)))

(use-package magit-gh-pulls
  :ensure t
  :defer t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (bind-key [f5] (lambda ()
                   "custom undo tree visualizer toggle"
                   (interactive)
                   (if (get-buffer undo-tree-visualizer-buffer-name)
                       (undo-tree-visualizer-quit)
                     (undo-tree-visualize)))))

(use-package smex
  :bind (("C-x C-m" . smex)
         ("C-c C-m" . smex)
         ("C-c C-M" . smex-major-mode-commands))
  :init
  (setq smex-flex-matching t)
  (setq smex-save-file (locate-user-emacs-file ".smex-items"))
  :config
  (smex-initialize))

(use-package company
  :init (setq company-idle-delay 0.2)
  :config
  (global-company-mode)
  (bind-key "TAB" #'company-complete-selection company-active-map)
  (bind-key "C-n" #'company-select-next company-active-map)
  (bind-key "C-p" #'company-select-previous company-active-map)
  (bind-key "C-," (lambda ()
                    (interactive)
                    (company-abort)
                    (completion-at-point)) company-active-map))

(use-package company-web
  :defer t
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package company-restclient
  :defer t
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-tern
  :defer t
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (dolist (hook '(js2-mode-hook
                  js-mode-hook
                  java-mode
                  restclient-mode-hook
                  ruby-mode
                  mark-down-mode))
    (add-hook hook 'turn-on-smartparens-mode)))

(use-package writeroom-mode)

(use-package remark-mode
  :load-path "site-lisp/remark-mode/"
  :commands remark-mode)

(use-package w3m
  :commands w3m
  :defer t
  :config
  (bind-key "M-n" nil w3m-mode-map))

(use-package browse-kill-ring
  :commands browse-kill
  :bind ("C-x C-y" . browse-kill))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

(use-package tagedit
  :config
  (bind-key "C-<left>" 'tagedit-forward-barf-tag html-mode-map)
  (bind-key "C-<right>" 'tagedit-forward-slurp-tag html-mode-map)
  (bind-key "C-k" 'tagedit-kill html-mode-map)
  (bind-key "M-k" 'tagedit-kill-attribute html-mode-map)
  (bind-key "M-r" 'tagedit-raise-tag html-mode-map)
  (bind-key "M-s" 'tagedit-splice-tag html-mode-map)
  (bind-key "M-S" 'tagedit-split-tag html-mode-map)
  (bind-key "M-J" 'tagedit-join-tags html-mode-map)

  (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
  (add-hook 'sgml-mode-hook (lambda () (bind-key "C-c C-r" 'mc/mark-sgml-tag-pair sgml-mode-map))))

(use-package ido
  :init
  ;; match arbitrary substrings
  (setq ido-enable-prefix nil)

  ;; ignore case when searching for buffers and file names
  (setq ido-case-fold t)

  ;; always new buffers if no buffer matches substring
  (setq ido-create-new-buffer 'always)

  ;; partial matches
  (setq ido-enable-flex-matching t)

  ;; only match in the current directory
  ;; (setq ido-auto-merge-work-directories-length -1)

  ;; show recently open files
  (setq ido-use-virtual-buffers t)

  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil)

  ;; allow alot of folders
  (setq ido-max-directory-size 100000)

  ;; prioritize some file types
  (setq ido-file-extensions-order '())

  (setq ido-save-directory-list-file (locate-user-emacs-file ".ido.last"))

  :config
  (ido-mode t)

  ;; in all contexts
  (ido-everywhere 1)

  ;; ignores
  (add-to-list 'ido-ignore-files "\\.DS_Store")
  (add-to-list 'ido-ignore-directories "target")
  (add-to-list 'ido-ignore-directories "node_modules")
  (add-hook 'ido-setup-hook '(lambda ()
                               ;; remove silly ido-toggle-ignore binding to c-a
                               (bind-key "C-a" nil ido-completion-map)
                               (bind-key "C-," 'ido-toggle-ignore ido-completion-map)
                               (bind-key "TAB" 'ido-exit-minibuffer ido-completion-map)

                               ;; c-w should go back up a dir - insert current file name with c-x c-w instead.
                               (bind-key "C-w" 'ido-delete-backward-updir ido-file-completion-map)
                               (bind-key "C-x C-w" 'ido-copy-current-file-name ido-file-completion-map)

                               (bind-key "C-w" 'ido-delete-backward-updir ido-file-dir-completion-map)
                               (bind-key "C-x C-w" 'ido-copy-current-file-name ido-file-dir-completion-map))))

(use-package flx-ido
  :config (flx-ido-mode 1))

(use-package ido-vertical-mode
  :init (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  :config (ido-vertical-mode))

(use-package ido-ubiquitous
  :config (ido-ubiquitous-mode 1))

(use-package ido-at-point
  :load-path "setup/" ;; the one on melpa is borked?
  :config (ido-at-point-mode 1))

(use-package visual-regexp
  :bind ("M-%" . vr/query-replace))

(use-package dash-at-point
  :commands dash-at-point
  :bind ("C-c C-j" . dash-at-point))

(use-package fancy-battery
  :if is-mac
  :config (fancy-battery-mode))

(use-package evil
  :commands evil-mode
  :init
  (setq evil-default-state 'emacs)
  (bind-key "C-'" '(lambda ()
                     (interactive)
                     (if (and (boundp 'evil-state)
                              (not (string-equal evil-state "emacs")))
                         (evil-emacs-state)
                       (progn
                         (evil-mode)
                         (evil-exit-emacs-state)))))
  :config
  (evil-mode 1)
  (evil-set-initial-state 'help-mode 'emacs)
  (message "evil"))

(use-package move-text
  :bind (("<C-S-up>" . move-text-up)
         ("<C-S-down>" . move-text-down)))

(use-package multiple-cursors
  :bind (("M-ø" . mc/mark-all-dwim)
         ("M-Ø" . mc/mark-more-like-this-extended)
         ("M-Å" . mc/mark-all-in-region)
         ("M-å" . mc/mark-previous-like-this)
         ("M-æ" . mc/mark-next-like-this)
         ("<C-S-mouse-1>" . mc/add-cursor-on-click)
         ("C-M-SPC" . set-rectangular-region-anchor)))

(use-package expand-region
  :config
  (bind-key (if is-mac "M-@" "M-'") 'er/expand-region))

(use-package transpose-frame
  :commands transpose-frame)

(use-package jump-char
  :init
  (setq jump-char-forward-key "m")
  (setq jump-char-backward-key "M")
  :config
  ;; ; to go forward and , to go back
  (global-set-key [(meta m)] 'jump-char-forward)
  (global-set-key [(shift meta m)] 'jump-char-backward))

(use-package etags-select
  :bind (("M-?" . ido-find-tag)
         ("M-." . find-tag-at-point)
         ("M-_" . find-tag-other-window))
  :config
  (add-hook 'etags-select-mode-hook (lambda ()
                                      (bind-key "RET" 'etags-select-goto-tag etags-select-mode-map)
                                      (bind-key "M-RET" 'etags-select-goto-tag-other-window etags-select-mode-map))))

(use-package yasnippet
  :diminish yas-minor-mode
  :defer t
  :init
  ;; use custom snippets
  (setq yas-snippet-dirs '(dir-snippets))
  ;; remove dropdowns
  (setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))
  ;; silence
  (setq yas-verbosity 1)
  ;; wrap around region
  (setq yas-wrap-around-region t)

  :config
  ;; on for all buffers
  (yas-global-mode 1)

  ;; jump to end of snippet definition
  (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

  ;; inter-field navigation
  (defun yas/goto-end-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-end (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-end-of-line 1)
        (goto-char position))))

  (defun yas/goto-start-of-active-field ()
    (interactive)
    (let* ((snippet (car (yas--snippets-at-point)))
           (position (yas--field-start (yas--snippet-active-field snippet))))
      (if (= (point) position)
          (move-beginning-of-line 1)
        (goto-char position))))

  (bind-key "C-e" 'yas/goto-end-of-active-field yas-keymap)
  (bind-key "C-a" 'yas/goto-start-of-active-field yas-keymap))

(use-package ag
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t
        ag-project-root-function (lambda (d) (projectile-project-root))))

(use-package wgrep)
(use-package wgrep-ag)

(use-package dired
  :ensure nil
  :commands dired-jump
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (setq wdired-allow-to-change-permissions t)
  :config
  (bind-key "C-x C-j" 'dired-jump)
  (bind-key "C-x M-j" '(lambda () (interactive) (dired-jump 1)))
  (bind-key "M-<up>" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-p" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-<down>" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map)
  (bind-key "M-n" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map))

;; less verbose dired
(use-package dired-details
  :init (setq-default dired-details-hidden-string "")
  :config (dired-details-install))

(use-package fill-column-indicator)

(use-package highlight-escape-sequences
  :config
  (hes-mode)
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-comment-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-comment-face))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :init
  (setq highlight-symbol-idle-delay 0.2)
  :config
  ;; TODO gruvbox colors
  (set-face-background 'highlight-symbol-face "#333")
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (bind-key "C-c s h" 'highlight-symbol)
  (bind-key "C-c s o" 'highlight-symbol-occur)
  (bind-key "C-c s n" 'highlight-symbol-next)
  (bind-key "C-c s p" 'highlight-symbol-prev)
  (bind-key "C-c s %" 'highlight-symbol-query-replace))

(use-package highlight-numbers
  :config
  (highlight-numbers-mode))

(use-package json-reformat)

(use-package restclient
  :mode "\\.http$")

(use-package hackernews
  :commands hackernews
  :defer t)

(use-package whitespace-cleanup-mode)

(require 'setup-shell)
(require 'setup-org)
(require 'setup-powerline)

(require 'keys)
(require 'langs)
(when is-mac (require 'mac))
(when is-cygwin (require 'cygwin))
