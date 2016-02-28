(let ((proxy-file (locate-user-emacs-file "proxy.el")))
  (when (file-exists-p proxy-file)
    (require 'proxy proxy-file)))

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
  (setq use-package-always-ensure t)

  ;; lexical-let
  (require 'cl))
(defvar use-package-verbose t)
(require 'diminish)
(require 'bind-key)

;; ;; benchmarks
;; (use-package benchmark-init
;;   :if t
;;   :config (benchmark-init/activate))

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

(require 'defuns)
(require 'sane-defaults)
(require 'setup-evil)

(use-package paradox
  :commands (paradox-list-packages paradox-upgrade-packages)
  :init
  (setq paradox-execute-asynchronously t))

(use-package exec-path-from-shell
  :if is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package rainbow-mode
  :commands rainbow-mode
  :defer t
  :config
  (dolist (mode-hook '(prog-mode-hook css-mode-hook html-mode-hook))
    (add-hook mode-hook #'rainbow-mode)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
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
  :commands (enable-paredit-mode evil-paredit-mode)
  :defer 1
  :config
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       eval-expression-minibuffer-setup-hook
                       ielm-mode-hook
                       lisp-mode-hook
                       lisp-interaction-mode-hook
                       scheme-mode-hook))
    (add-hook mode-hook #'enable-paredit-mode)
    (add-hook mode-hook #'evil-paredit-mode))
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       lisp-interaction-mode-hook
                       ielm-mode-hook))
    (add-hook mode-hook 'turn-on-eldoc-mode)))

(use-package which-key
  :diminish which-key-mode
  :config
  ;; percentage height
  (setq which-key-side-window-max-height 0.5)
  (setq which-key-separator " ")
  (setq which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎")))

  ;; time to wait before display
  (setq which-key-idle-delay 0.4)
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
  :defer 2
  :config
  (projectile-global-mode)
  (evil-leader/set-key "t" 'helm-projectile)
  (t/declare-prefix "p" "Project"
                    "b" 'helm-browse-project
                    "c" 'projectile-switch-project
                    "d" 'projectile-dired
                    "k" 'projectile-kill-buffers
                    "o" 't/open-in-desktop
                    "R" 'projectile-replace
                    "s" 'projectile-save-project-buffers)

  (t/declare-prefix "pr" "Project run"
                    "a" 'projectile-run-async-shell-command-in-root
                    "s" 'projectile-run-shell-command-in-root)

  (t/declare-prefix "pf" "Project find"
                    "d" 'projectile-find-dir
                    "f" 'projectile-find-file-dwim
                    "p" 'projectile-find-file-in-known-projects
                    "t" 'projectile-find-test-file))

(use-package neotree
  :bind (([f6] . neotree-toggle))
  :init
  (setq neo-window-width 30
        neo-smart-open nil
        neo-create-file-auto-open t
        neo-show-updir-line nil
        neo-dont-be-alone t
        neo-auto-indent-point t)
  (when is-mac (setq neo-theme 'nerd))
  :config
  (add-hook 'neotree-mode-hook
            (lambda ()
              (defun neotree-change-root-up () (interactive) (neotree-select-up-node))
              (bind-key "n" 'neotree-next-line evil-normal-state-local-map)
              (bind-key "p" 'neotree-previous-line evil-normal-state-local-map)
              (bind-key "C-n" 'neotree-next-line evil-normal-state-local-map)
              (bind-key "C-p" 'neotree-previous-line evil-normal-state-local-map)
              (bind-key "c" 'neotree-create-node evil-normal-state-local-map)
              (bind-key "R" 'neotree-rename-node evil-normal-state-local-map)
              (bind-key "D" 'neotree-delete-node evil-normal-state-local-map)
              (bind-key "i" 'neotree-enter-horizontal-split evil-normal-state-local-map)
              (bind-key "s" 'neotree-enter-vertical-split evil-normal-state-local-map)
              (bind-key "g" 'neotree-refresh evil-normal-state-local-map)
              (bind-key "u" 'neotree-change-root-up evil-normal-state-local-map)
              (bind-key "I" 'neotree-hidden-file-toggle evil-normal-state-local-map)
              (bind-key "q" 'neotree-hide evil-normal-state-local-map)
              (bind-key "RET" 'neotree-enter evil-normal-state-local-map)
              (bind-key "C" 'neotree-change-root evil-normal-state-map))))

(t/declare-prefix "g" "Git")

(use-package git-gutter+
  :defer 1
  :diminish git-gutter+-mode
  :config
  (setq git-gutter+-modified-sign "~")
  (setq git-gutter+-added-sign "+")
  (setq git-gutter+-deleted-sign "-")
  (setq git-gutter+-separator-sign (if has-gui "" " "))
  (global-git-gutter+-mode t)
  (t/declare-prefix "gh" "Hunk"
                    "n" 'git-gutter+-next-hunk
                    "N" 'git-gutter+-previous-hunk
                    "C" 'git-gutter+-stage-and-commit
                    "?" 'git-gutter+-show-hunk-inline-at-point
                    "=" 'git-gutter+-show-hunk
                    "r" 'git-gutter+-revert-hunks
                    "s" 'git-gutter+-stage-hunks
                    "c" 'git-gutter+-commit))

(use-package git-gutter-fringe+
  :defer 1
  :if has-gui
  :config
  (git-gutter+-enable-fringe-display-mode))

(use-package helm-open-github
  :config
  (t/declare-prefix "go" "Open github"
                    "i" 'helm-open-github-from-issues
                    "c" 'helm-open-github-from-commit
                    "f" 'helm-open-github-from-file
                    "p" 'helm-open-github-from-pull-requests))

(use-package git-timemachine
  :commands git-timemachine-toggle
  :init
  (evil-leader/set-key "gT" 'git-timemachine-toggle)
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    (when git-timemachine-mode
      (bind-key "C-n" 'git-timemachine-show-next-revision evil-normal-state-local-map)
      (bind-key "C-p" 'git-timemachine-show-previous-revision evil-normal-state-local-map))))

(use-package gist
  :defer 2
  :init
  (t/declare-prefix "gg" "Gist"
                    "l" 'gist-list
                    "b" 'gist-buffer
                    "B" 'gist-buffer-private
                    "r" 'gist-region
                    "R" 'gist-region-private))

(use-package linum-relative
  :commands (linum-relative-mode linum-mode)
  :config
  (linum-relative-mode))

(use-package ace-window
  :commands ace-window
  :init
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
        aw-background t)
  :config
  (t/declare-prefix "j" "Jump to"
                    "w" 'ace-window))

(use-package ace-jump-mode
  :commands (ace-jump-mode ace-jump-char-mode ace-jump-line-mode ace-jump-word-mode)
  :init
  (setq ace-jump-mode-gray-background t
        ace-jump-mode-case-fold t)
  (t/declare-prefix "j" "Jump to"
                    "j" 'ace-jump-mode
                    "c" 'ace-jump-char-mode
                    "l" 'ace-jump-line-mode
                    "W" 'ace-jump-word-mode))

(use-package magit
  :commands magit-status
  :defer t
  :init
  (t/declare-prefix "g" "Git"
                    "s" 'magit-status
                    "b" 'magit-blame
                    "l" 'magit-log
                    "C" 'magit-commit)
  :config
  (bind-key "q" #'magit-quit-session magit-status-mode-map)

  (defadvice magit-blame-mode (after switch-to-emacs-mode activate)
    (if magit-blame-mode
        (evil-emacs-state 1)
      (evil-normal-state 1)))

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
  :defer 2
  :commands global-company-mode
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
  :defer 2
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-web-html)))

(use-package company-ansible
  :defer 2
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-ansible)))

(use-package company-restclient
  :defer 2
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-restclient)))

(use-package company-tern
  :defer 2
  :init
  (setq tern-command (append tern-command '("--no-port-file")))
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(use-package company-emoji
  :defer 2
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-emoji)))

(use-package smartparens
  :diminish smartparens-mode
  :commands turn-on-smartparens-mode
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
                                '((t/sp--create-newline-and-enter-sexp "RET")))
                 (sp-local-pair "{" nil :post-handlers
                                '((t/sp--create-newline-and-enter-sexp "RET")))))

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

    (add-hook 'helm-before-initialize-hook 'neotree-hide)

    (bind-key "C-w" 'backward-kill-word helm-map)

    (use-package helm-ag
      :commands helm-ag
      :defer t
      :init
      (setq helm-ag-fuzzy-match t
            ;; save edited buffers on completion
            helm-ag-edit-save t))

    (use-package helm-projectile
      :commands helm-projectile
      :defer t)

    (use-package helm-descbinds
      :commands helm-descbinds
      :init
      (helm-descbinds-mode))

    (use-package helm-swoop)))

(use-package visual-regexp
  :commands vr/query-replace
  :bind ("M-%" . vr/query-replace))

(use-package dash-at-point
  :commands dash-at-point
  :bind ("C-c C-j" . dash-at-point))

(use-package fancy-battery
  :defer 2
  :if is-mac
  :config (fancy-battery-mode))

(use-package move-text
  :commands (move-text-up move-text-down)
  :bind (("<C-S-up>" . move-text-up)
         ("<C-S-down>" . move-text-down)))

(use-package multiple-cursors)

(use-package expand-region
  :config
  (bind-key (if is-mac "M-@" "M-'") 'er/expand-region)
  (bind-key (if is-mac "M-*" "M-§") 'er/contract-region))

(use-package transpose-frame
  :commands transpose-frame)

(use-package etags-select
  :bind (("M-?" . t/ido-find-tag)
         ("M-." . t/find-tag-at-point)
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
  :commands ag
  :config
  (setq ag-reuse-buffers t
        ag-highlight-search t
        ag-project-root-function (lambda (d) (projectile-project-root))))

(use-package wgrep
  :defer t)
(use-package wgrep-ag
  :defer t)

(use-package autorevert
  :init
  ;; silenced refresh of dired
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1)
  (when is-mac
    ;; file notifications aren't supported on os x
    (setq auto-revert-use-notify nil)))

(use-package dired
  :ensure nil
  :commands dired-jump
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (setq wdired-allow-to-change-permissions t)
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks "@")
  :config
  (bind-key "C-c C-e" 'dired-toggle-read-only)
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

;; Enforce proper whitespace
(use-package ethan-wspace
  :init
  (setq mode-require-final-newline nil)
  (setq require-final-newline nil)
  :config
  (global-ethan-wspace-mode 1))

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
  (set-face-background 'highlight-symbol-face "#383838")
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (t/declare-prefix "h" "Highlight"
                    ;; leader leader clears all highlights
                    "h" 'highlight-symbol-at-point
                    "n" 'highlight-symbol-next
                    "N" 'highlight-symbol-prev))

(use-package highlight-numbers
  :config
  (highlight-numbers-mode 1))

(use-package restclient
  :commands restclient-mode
  :mode "\\.http$")

(use-package hackernews
  :commands hackernews
  :defer t)

(use-package geeknote
  :init
  (setq geeknote-command "python /usr/local/bin/geeknote")
  :config
  (t/declare-prefix "gn" "Geeknote"
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
  (t/declare-prefix "o" "Other"
                    "s" 'helm-spotify))

(use-package calendar
  :init
  (setq calendar-week-start-day 1)
  (t/declare-prefix "o" "Other"
                    "c" 'calendar))

(require 'setup-shell)
(require 'setup-org)
(require 'setup-powerline)

(require 'keys)
(require 'langs)
(when is-mac (require 'mac))
(when is-cygwin (require 'cygwin))

(when (or is-cygwin
          (display-graphic-p))
  (require 'server)
  (unless (server-running-p) (server-mode)))

(bind-key "C-x C-c" 't/delete-frame-or-hide-last-remaining-frame)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'spacemacs-dark t))))
  (load-theme 'spacemacs-dark t))

(evil-mode 1)

;; custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; ;; benchmarks
;; (benchmark-init/show-durations-tabulated)
;; (benchmark-init/show-durations-tree)
