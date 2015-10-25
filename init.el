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
  :if nil
  :ensure nil)

;; packages

(setq is-mac (equal system-type 'darwin))
(setq is-cygwin (equal system-type 'cygwin))
(setq is-linux (equal system-type 'gnu/linux))
(setq is-win (equal system-type 'windows-nt))
(setq has-gui (display-graphic-p))

(defvar leader "SPC")

(use-package dash)
(use-package s)
(use-package better-defaults)

(require 'sane-defaults)
(require 'defuns)
(require 'setup-evil)

(use-package paradox
  :init
  (progn
    (setq paradox-execute-asynchronously t)))

(use-package exec-path-from-shell
  :if is-mac
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
    (add-hook mode-hook 'turn-on-eldoc-mode)))

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
  (setq which-key-idle-delay 0.2)
  (which-key-mode nil)
  (which-key-mode))

(use-package subword
  :defer t
  :ensure nil
  :config (subword-mode))

(use-package projectile
  :init
  (setq projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld")
        projectile-completion-system 'helm)
  :config
  (projectile-global-mode)
  (evil-leader/set-key "t" 'helm-projectile)
  (declare-prefix "p" "Project"
                  "c" 'projectile-switch-project
                  "d" 'projectile-dired
                  "k" 'projectile-kill-buffers
                  "o" 'open-in-desktop
                  "R" 'projectile-replace
                  "s" 'projectile-save-project-buffers)

  (declare-prefix "pr" "Project run"
                  "a" 'projectile-run-async-shell-command-in-root
                  "s" 'projectile-run-shell-command-in-root)

  (declare-prefix "pf" "Project find"
                  "d" 'projectile-find-dir
                  "f" 'projectile-find-file-dwim
                  "p" 'projectile-find-file-in-known-projects
                  "t" 'projectile-find-test-file))

(use-package neotree
  :commands neotree
  :bind (([f6] . neotree-toggle))
  :init
  (setq neo-window-width 30)
  (setq neo-smart-open nil)
  (when is-mac (setq neo-theme 'nerd))
  (evil-leader/set-key "fl" 'neotree-find)
  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (defun neotree-change-root-up ()
                (interactive)
                (neotree-select-up-node)
                (neotree-select-up-node))
              (bind-key "n" 'neotree-next-line evil-normal-state-local-map)
              (bind-key "p" 'neotree-previous-line evil-normal-state-local-map)
              (bind-key "C-n" 'neotree-next-line evil-normal-state-local-map)
              (bind-key "C-p" 'neotree-previous-line evil-normal-state-local-map)
              (bind-key "c" 'neotree-create-node evil-normal-state-local-map)
              (bind-key "r" 'neotree-rename-node evil-normal-state-local-map)
              (bind-key "d" 'neotree-delete-node evil-normal-state-local-map)
              (bind-key "i" 'neotree-enter-horizontal-split evil-normal-state-local-map)
              (bind-key "s" 'neotree-enter-vertical-split evil-normal-state-local-map)
              (bind-key "g" 'neotree-refresh evil-normal-state-local-map)
              (bind-key "u" 'neotree-change-root-up evil-normal-state-local-map)
              (bind-key "I" 'neotree-hidden-file-toggle evil-normal-state-local-map)
              (bind-key "q" 'neotree-hide evil-normal-state-local-map)
              (bind-key "RET" 'neotree-enter evil-normal-state-local-map)
              (bind-key "C" 'neotree-change-root evil-normal-state-map))))

(declare-prefix "g" "Git")

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (setq git-gutter+-modified-sign "  ")
  (setq git-gutter+-added-sign "+ ")
  (setq git-gutter+-deleted-sign "- ")
  (global-git-gutter+-mode t)
  (declare-prefix "gh" "Hunk"
                  "n" 'git-gutter+-next-hunk
                  "N" 'git-gutter+-previous-hunk
                  "C" 'git-gutter+-stage-and-commit
                  "=" 'git-gutter+-show-hunk
                  "r" 'git-gutter+-revert-hunks
                  "s" 'git-gutter+-stage-hunks
                  "c" 'git-gutter+-commit)
  (use-package git-gutter-fringe+
    :if has-gui
    :config
    (git-gutter+-enable-fringe-display-mode)))

(use-package git-timemachine
  :commands git-timemachine
  :defer t
  :init
  (evil-leader/set-key "gT" 'git-timemachine-toggle)
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    (when git-timemachine-mode
      (bind-key "C-n" 'git-timemachine-show-next-revision evil-normal-state-local-map)
      (bind-key "C-p" 'git-timemachine-show-previous-revision evil-normal-state-local-map))))

(use-package gist
  :init
  (declare-prefix "gg" "Gist"
                  "l" 'gist-list
                  "b" 'gist-buffer
                  "B" 'gist-buffer-private
                  "r" 'gist-region
                  "R" 'gist-region-private))

(use-package linum-relative
  :config
  (linum-relative-mode))

(use-package ace-jump-mode
  :init
  (setq ace-jump-mode-gray-background nil
        ace-jump-mode-case-fold t)
  (declare-prefix "j" "Jump"
                  "j" 'ace-jump-mode
                  "c" 'ace-jump-char-mode
                  "l" 'ace-jump-line-mode
                  "w" 'ace-jump-word-mode))


(use-package magit
  :commands magit-status
  :defer t
  :init
  (declare-prefix "g" "Git"
                  "s" 'magit-status
                  "b" 'magit-blame-mode
                  "l" 'magit-log
                  "C" 'magit-commit)
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
    (git-gutter+-refresh))

  (use-package magit-gh-pulls
    :ensure t
    :defer t
    :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls)))

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
         ("C-c C-M" . smex-major-mode-commands))
  :init
  (setq smex-flex-matching t)
  (setq smex-save-file (locate-user-emacs-file ".smex-items"))
  :config
  (smex-initialize))

(use-package company
  :init
  (setq company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        ;; nav with m-<n>
        company-show-numbers t
        company-selection-wrap-around t
        company-require-match nil)

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
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-web-html)))

(use-package company-ansible
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ansible)))

(use-package company-restclient
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(use-package company-tern
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(use-package company-emoji
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-emoji)))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (dolist (hook '(js2-mode-hook
                  js-mode-hook
                  java-mode
                  restclient-mode-hook
                  ruby-mode
                  mark-down-mode))
    (add-hook hook 'turn-on-smartparens-mode))

  (require 'smartparens-config)
  (bind-key "<delete>" 'sp-delete-char sp-keymap)
  (bind-key "C-<right>" 'sp-forward-slurp-sexp sp-keymap)
  (bind-key "C-<left>" 'sp-forward-barf-sexp sp-keymap)
  (bind-key "C-S-<right>" 'sp-backward-barf-sexp sp-keymap)
  (bind-key "C-S-<left>" 'sp-backward-slurp-sexp sp-keymap)
  (bind-key "M-s" 'sp-unwrap-sexp sp-keymap)
  (bind-key "M-S-s" 'sp-raise-sexp sp-keymap)
  (bind-key "M-i" 'sp-split-sexp sp-keymap)
  (bind-key "M-S-i" 'sp-join-sexp sp-keymap)
  (bind-key "M-t" 'sp-transpose-sexp sp-keymap)
  (bind-key "M-S-<left>" 'sp-backward-sexp sp-keymap)
  (bind-key "M-S-<right>" 'sp-forward-sexp sp-keymap)

  (sp-with-modes '(java-mode
                   restclient-mode
                   ruby-mode
                   mark-down-mode
                   c-mode c++-mode
                   js-mode js2-mode)
    (sp-local-pair "[" nil :post-handlers
                   '((sp--create-newline-and-enter-sexp "RET")))
    (sp-local-pair "{" nil :post-handlers
                   '((sp--create-newline-and-enter-sexp "RET")))))

(use-package writeroom-mode
  :commands writeroom-mode
  :defer t)

(use-package w3m
  :commands w3m
  :defer t
  :config
  (bind-key "M-n" nil w3m-mode-map))

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

(use-package discover-my-major
  :defer t
  :commands (discover-my-major discover-my-mode))

(use-package remark-mode
  :commands remark-mode
  :defer t)

(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  (setq-default helm-display-header-line nil
                helm-M-x-fuzzy-match t
                helm-mode-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-projectile-fuzzy-match t
                helm-completion-in-region-fuzzy-match t

                ;; keep follow mode on, after on once
                helm-follow-mode-persistent t
                helm-ff-skip-boring-files t
                helm-quick-update t
                helm-M-x-requires-pattern nil)
  (helm-mode)
  (set-face-attribute 'helm-source-header nil :height 1)
  :config
  (progn

    (bind-key "C-w" 'backward-kill-word helm-map)

    (use-package helm-ag
      :defer t
      :init
      (setq helm-ag-fuzzy-match t
            ;; save edited buffers on completion
            helm-ag-edit-save t))

    (use-package helm-projectile
      :defer t)

    (use-package helm-descbinds
      :init
      (helm-descbinds-mode))

    (use-package helm-swoop)))

(use-package visual-regexp
  :bind ("M-%" . vr/query-replace))

(use-package dash-at-point
  :commands dash-at-point
  :bind ("C-c C-j" . dash-at-point))

(use-package fancy-battery
  :if is-mac
  :config (fancy-battery-mode))

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

(use-package etags-select
  :bind (("M-?" . ido-find-tag)
         ("M-." . find-tag-at-point)
         ("M-_" . find-tag-other-window))
  :config
  (add-hook 'etags-select-mode-hook
            (lambda ()
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

(use-package wgrep
  :defer t)
(use-package wgrep-ag
  :defer t)

(use-package dired
  :ensure nil
  :commands dired-jump
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (setq wdired-allow-to-change-permissions t)
  ;; auto-refresh, silently
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)
  :config
  (bind-key "C-x C-j" 'dired-jump)
  (bind-key "C-x M-j" '(lambda () (interactive) (dired-jump 1)))
  (bind-key "u" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-<up>" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-p" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-<down>" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map)
  (bind-key "M-n" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map))

;; less verbose dired
(use-package dired-details
  :init (setq-default dired-details-hidden-string "")
  :config (dired-details-install))

(use-package fill-column-indicator
  :defer t)

(use-package highlight-escape-sequences
  :config
  (put 'hes-escape-backslash-face 'face-alias 'font-lock-comment-face)
  (put 'hes-escape-sequence-face 'face-alias 'font-lock-comment-face)
  (hes-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :init
  (setq highlight-symbol-idle-delay 0.2)
  :config
  (set-face-background 'highlight-symbol-face "#333")
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (declare-prefix "h" "Highlight"
                  ;; leader leader clears all highlights
                  "h" 'highlight-symbol-at-point
                  "n" 'highlight-symbol-next
                  "N" 'highlight-symbol-prev))

(use-package highlight-numbers
  :config
  (highlight-numbers-mode))

(use-package restclient
  :mode "\\.http$")

(use-package hackernews
  :commands hackernews
  :defer t)

(use-package geeknote
  :init
  (setq geeknote-command "python /usr/local/bin/geeknote")
  :config
  (declare-prefix "gn" "Geeknote"
                  "c" 'geeknote-create
                  "e" 'geeknote-edit
                  "f" 'geeknote-find
                  "s" 'geeknote-show
                  "r" 'geeknote-remove
                  "m" 'geeknote-move))

(use-package spotify
  :ensure nil
  :load-path "site-lisp/spotify/"
  :commands helm-spotify
  :init
  (declare-prefix "o" "Other"
                  "s" 'helm-spotify))

(require 'setup-shell)
(require 'setup-org)
(require 'setup-powerline)

(require 'keys)
(require 'langs)
(when is-mac (require 'mac))
(when is-cygwin (require 'cygwin))

(when (display-graphic-p)
  (require 'server)
  (unless (server-running-p) (server-mode)))

(evil-leader/set-key "rq" 'save-buffers-kill-terminal)
(bind-key "C-x r q" 'save-buffers-kill-terminal)
(bind-key "C-x C-c" 'delete-frame-or-hide-last-remaining-frame)
(eval-after-load "evil"
  '(progn
     (defadvice evil-quit (around advice-for-evil-quit activate) (message "really?"))
     (defadvice evil-quit-all (around advice-for-evil-quit-all activate) (message "really?"))))

(use-package spacemacs-theme
  :init (load-theme 'spacemacs-dark t))

;; custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
