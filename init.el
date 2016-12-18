;; credits spacemacs for all the clever stuff

;;(package-initialize)

(load (locate-user-emacs-file "t-before.el") t)

(setq *user-dir-snippets* (locate-user-emacs-file "snippets")
      *user-dir-setup* (locate-user-emacs-file "setup")
      *user-dir-site-lisp* (locate-user-emacs-file "site-lisp"))

(add-to-list 'load-path *user-dir-setup*)
(add-to-list 'load-path *user-dir-site-lisp*)

;; add folders inside site-lisp as well
(dolist (project (directory-files *user-dir-site-lisp* t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 't-packaging)

(defvar t-debug-init nil "Debug/time startup")
(when t-debug-init
  (message "t: timing init")
  (require 't-debug)

  ;; benchmarks
  (use-package benchmark-init
    :config (benchmark-init/activate)))

(setq is-mac (equal system-type 'darwin))
(setq is-cygwin (equal system-type 'cygwin))
(setq is-linux (equal system-type 'gnu/linux))
(setq is-win (equal system-type 'windows-nt))
(setq is-ms (or is-cygwin is-win))
(setq has-gui (display-graphic-p))

(defvar *user-leader* "SPC")

(require 't-defuns)
(require 't-sane-defaults)

(require 't-which-key)
(require 't-evil)
(evil-mode 1)

(use-package paradox
  :commands (paradox-list-packages paradox-upgrade-packages)
  :init
  (setq paradox-github-token t
        paradox-execute-asynchronously t))

(use-package exec-path-from-shell
  :if is-mac
  :config
  (exec-path-from-shell-initialize))

(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode)

(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (dolist (mode-hook '(prog-mode-hook css-mode-hook html-mode-hook))
    (add-hook mode-hook #'rainbow-mode)))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package tramp
  :ensure nil
  :defer 1
  :config
  (setq tramp-auto-save-directory (locate-user-emacs-file ".tramp-auto-save")))

(use-package selectric-mode
  :commands selectric-mode)

(use-package paredit
  :diminish paredit-mode
  :commands (enable-paredit-mode evil-cleverparens-mode)
  :init
  (dolist (mode-hook '(emacs-lisp-mode-hook
                       clojure-mode-hook
                       eval-expression-minibuffer-setup-hook
                       ielm-mode-hook
                       lisp-mode-hook
                       lisp-interaction-mode-hook
                       scheme-mode-hook))
    (add-hook mode-hook #'enable-paredit-mode)
    (add-hook mode-hook #'evil-cleverparens-mode)))

(use-package subword
  :diminish subword-mode
  :defer 1
  :ensure nil
  :config (subword-mode))

(use-package projectile
  :diminish projectile-mode
  :commands (projectile-mode helm-projectile)
  :init
  (setq projectile-mode-line '(:eval (format "[%s]" (projectile-project-name)))
        projectile-known-projects-file (locate-user-emacs-file ".projectile-bookmarks.eld")
        projectile-completion-system 'helm)
  (evil-leader/set-key "t" 'helm-projectile)
  :config
  (add-to-list 'projectile-globally-ignored-directories "elpa-backups")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories "target")
  (add-to-list 'projectile-globally-ignored-directories "dist")
  (add-to-list 'projectile-globally-ignored-directories ".idea")
  (add-to-list 'projectile-globally-ignored-files "**.bundle.js")
  (add-to-list 'projectile-globally-ignored-files "**.build.js")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (add-to-list 'grep-find-ignored-files "**.bundle.js")
  (add-to-list 'grep-find-ignored-files "**.build.js")
  (add-to-list 'grep-find-ignored-files ".DS_Store")
  (projectile-global-mode)
  (t/declare-prefix "p" "Project"
                    "b" 'helm-browse-project
                    "c" 'projectile-switch-project
                    "d" 'projectile-find-dir
                    "f" 'projectile-find-file-dwim
                    "G" 'projectile-regenerate-tags
                    "k" 'projectile-kill-buffers
                    "l" 'neotree-find-project-root
                    "o" 't/open-in-desktop
                    "p" 'projectile-find-file-in-known-projects
                    "R" 'projectile-replace
                    "S" 'projectile-save-project-buffers
                    "t" 'projectile-find-test-file)

  (t/declare-prefix "pr" "Project run"
                    "a" 'projectile-run-async-shell-command-in-root
                    "s" 'projectile-run-shell-command-in-root))

(use-package neotree
  :bind (([f6] . neotree-toggle))
  :init
  (setq neo-window-width 30
        neo-smart-open nil
        neo-create-file-auto-open t
        neo-show-updir-line nil
        neo-dont-be-alone t
        neo-show-hidden-files t
        neo-auto-indent-point t)
  (when is-mac (setq neo-theme 'icons))
  :config

  (defun neotree-find-project-root ()
    (interactive)
    (if (neo-global--window-exists-p)
        (neotree-hide)
      (let ((origin-buffer-file-name (buffer-file-name)))
        (neotree-find (projectile-project-root))
        (neotree-find origin-buffer-file-name))))

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
              (bind-key "q" 'neotree-hide evil-normal-state-local-map)
              (bind-key "TAB" 'neotree-stretch-toggle evil-normal-state-local-map)
              (bind-key "RET" 'neotree-enter evil-normal-state-local-map)
              (bind-key "C" 'neotree-change-root evil-normal-state-local-map))))

(use-package nlinum
  :defer t
  :config
  (setq nlinum-format "%4d "
        nlinum-highlight-current-line t))

(use-package nlinum-relative
  :init
  (setq nlinum-relative-redisplay-delay 0
        nlinum-relative-current-symbol "> ")
  :config
  (nlinum-relative-setup-evil))


(t/declare-prefix "g" "Git")

(use-package git-gutter+
  :defer 1
  :diminish git-gutter+-mode
  :config
  (setq git-gutter+-modified-sign "~"
        git-gutter+-added-sign "+"
        git-gutter+-deleted-sign "-"
        git-gutter+-separator-sign (if has-gui "" " "))
  (global-git-gutter+-mode t)
  (t/declare-prefix "gh" "Hunk"
                    "n" 'git-gutter+-next-hunk
                    "N" 'git-gutter+-previous-hunk
                    "C" 'git-gutter+-stage-and-commit
                    "?" 'git-gutter+-show-hunk-inline-at-point
                    "=" 'git-gutter+-show-hunk
                    "r" 'git-gutter+-revert-hunks
                    "s" 'git-gutter+-stage-hunks
                    "cc" 'magit-commit
                    "ca" 'magit-commit-amend))

(use-package git-gutter-fringe+
  :after git-gutter+
  :if has-gui
  :config
  (git-gutter+-enable-fringe-display-mode)
  (fringe-helper-define 'git-gutter-fr+-added nil
    "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX.....")
  (fringe-helper-define 'git-gutter-fr+-deleted nil
    "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX.....")
  (fringe-helper-define 'git-gutter-fr+-modified nil
    "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....." "....XXXX....."))

(use-package helm-open-github
  :commands (helm-open-github-from-issues
             helm-open-github-from-commit
             helm-open-github-from-file
             helm-open-github-from-pull-requests)
  :init
  (t/declare-prefix "go" "Open github"
                    "i" 'helm-open-github-from-issues
                    "c" 'helm-open-github-from-commit
                    "f" 'helm-open-github-from-file
                    "p" 'helm-open-github-from-pull-requests))

(use-package git-link
  :commands git-link
  :init
  (setq git-link-open-in-browser t)
  (t/declare-prefix "go" "Open github"
                    "l" 'git-link))

(use-package git-timemachine
  :commands git-timemachine-toggle
  :init
  (t/declare-prefix "g" "Git"
                    "T" 'git-timemachine-toggle)
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    (when git-timemachine-mode
      (bind-key "C-n" 'git-timemachine-show-next-revision evil-normal-state-local-map)
      (bind-key "C-p" 'git-timemachine-show-previous-revision evil-normal-state-local-map))))

(use-package gist
  :commands (gist-list
             gist-buffer
             gist-buffer-private
             gist-region
             gist-region-private)
  :init
  (t/declare-prefix "gg" "Gist"
                    "l" 'gist-list
                    "b" 'gist-buffer
                    "B" 'gist-buffer-private
                    "r" 'gist-region
                    "R" 'gist-region-private))

(use-package ace-window
  :commands ace-window
  :init
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
        aw-background t)
  (t/declare-prefix "j" "Jump to"
                    "w" 'ace-window))

(use-package ace-jump-mode
  :commands (ace-jump-mode
             ace-jump-char-mode
             ace-jump-line-mode
             ace-jump-word-mode)
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
  :init
  (t/declare-prefix "g" "Git"
                    "s" 'magit-status
                    "b" 'magit-blame
                    "l" 'magit-log
                    "C" 'magit-commit)

  (setq magit-pull-arguments nil
        magit-fetch-arguments '("--prune")
        magit-rebase-arguments '("--interactive")
        magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
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
    (git-gutter+-refresh)))

(use-package magit-gh-pulls
  :after magit
  :ensure t
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (bind-key [f7] (lambda ()
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
  :diminish company-mode
  :defer 1
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
  :after company
  :config
  (add-to-list 'company-backends 'company-web-html))

(use-package company-ansible
  :after company
  :config
  (add-to-list 'company-backends 'company-ansible))

(use-package company-restclient
  :after company
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-tern
  :diminish tern-mode
  :after company
  :bind (:map
         tern-mode-keymap
         ("M-." . nil)
         ("M-," . nil)
         ("C-M-." . nil)
         ("M-," . pop-tag-mark)
         ("C-M-." . helm-etags-select))
  :config
  (add-to-list 'company-backends 'company-tern)
  (setq tern-command (append tern-command '("--no-port-file"))))

(use-package company-emoji
  :after company
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package smartparens
  :diminish smartparens-mode
  :commands turn-on-smartparens-mode
  :init
  (dolist (hook '(js2-mode-hook
                  js-mode-hook
                  java-mode
                  text-mode-hook
                  restclient-mode-hook
                  ruby-mode
                  mark-down-mode))
    (add-hook hook 'turn-on-smartparens-mode))
  :config
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

  (sp-with-modes '(js2-mode
                   js-mode
                   java-mode
                   text-mode
                   restclient-mode
                   ruby-mode
                   mark-down-mode)
    (sp-local-pair "[" nil :post-handlers
                   '((t/sp--create-newline-and-enter-sexp "RET")))
    (sp-local-pair "{" nil :post-handlers
                   '((t/sp--create-newline-and-enter-sexp "RET")))))

(use-package writeroom-mode
  :commands writeroom-mode
  :init
  (t/declare-prefix "T" "Toggle"
                    "w" 'writeroom-mode))

(use-package w3m
  :commands w3m
  :bind (:map
         w3m-mode-map
         ("M-p" . backward-paragraph)
         ("M-n" . forward-paragraph)))

(use-package discover-my-major
  :commands (discover-my-major discover-my-mode))

(use-package remark-mode
  :commands remark-mode)

(use-package helm
  :commands (helm-mini helm-projectile helm-projectile-ag)
  :diminish helm-mode
  :bind (:map
         helm-map
         ("C-w" . backward-kill-word)
         ("C-u" . backward-kill-sentence)
         ("C-c u" . universal-argument))

  :init
  (require 'helm-config)
  (setq-default helm-display-header-line nil
                helm-M-x-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-completion-in-region-fuzzy-match t
                helm-file-cache-fuzzy-match t
                helm-lisp-fuzzy-completion t
                helm-mode-fuzzy-match t
                helm-projectile-fuzzy-match t
                helm-recentf-fuzzy-match t
                helm-candidate-number-limit 100
                helm-prevent-escaping-from-minibuffer t
                helm-always-two-windows t
                helm-echo-input-in-header-line t

                ;; keep follow mode on, after on once
                helm-follow-mode-persistent t
                helm-ff-skip-boring-files t
                helm-quick-update t
                helm-M-x-requires-pattern nil)


  (defun t/hide-cursor-in-helm-buffer ()
    "Hide the cursor in helm buffers."
    (with-helm-buffer
      (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 't/hide-cursor-in-helm-buffer)
  :config
  (progn
    (helm-mode 1)
    (set-face-attribute 'helm-source-header nil :height 1)
    (add-hook 'helm-before-initialize-hook 'neotree-hide)))

(use-package helm-ag
  :after helm
  :commands helm-ag
  :init
  (setq helm-ag-fuzzy-match t
        helm-ag-insert-at-point 'symbol
        helm-ag-use-grep-ignore-list t
        ;; save edited buffers on completion
        helm-ag-edit-save t)
  (when is-ms
    (setq helm-ag-base-command "ag --nocolor --nogroup --vimgrep")))

(use-package helm-projectile
  :after helm
  :commands helm-projectile)

(use-package helm-descbinds
  :after helm
  :commands helm-descbinds
  :init
  (helm-descbinds-mode)
  (setq helm-descbinds-window-style 'split))

(use-package helm-dash
  :after helm
  :commands helm-dash)

(use-package helm-swoop
  :after helm
  :commands helm-swoop
  :bind (:map
         helm-swoop-edit-map
         ("C-c C-c" . helm-swoop--edit-complete)
         ("C-c C-k" . helm-swoop--edit-cancel)))

(use-package visual-regexp
  :commands vr/query-replace
  :bind ("M-%" . vr/query-replace))

(use-package dash-at-point
  :commands dash-at-point
  :bind ("C-c C-j" . dash-at-point))

(use-package fancy-battery
  :defer t
  :if is-mac)

(bind-key "<C-S-up>" 't/move-line-up)
(bind-key "<C-S-down>" 't/move-line-down)

(use-package expand-region
  :commands (er/expand-region er/contract-region)
  :init
  (bind-key (if is-mac "M-@" "M-'") 'er/expand-region)
  (bind-key (if is-mac "M-*" "M-§") 'er/contract-region))

(use-package transpose-frame
  :commands transpose-frame)

(use-package dockerfile-mode
  :mode "^Dockerfile$")

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
  :init
  (setq yas-snippet-dirs '(*user-dir-snippets*)
        ;; remove dropdowns
        yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
        ;; silence
        yas-verbosity 1
        ;; wrap around region
        yas-wrap-around-region t)

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
  (setq auto-revert-interval 1
        ;; silenced refresh of dired
        auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode)
  (when is-mac
    ;; file notifications aren't supported on os x
    (setq auto-revert-use-notify nil)))

(use-package smooth-scrolling
  :defer 1
  :init
  (setq smooth-scroll-margin 5
        mouse-wheel-scroll-amount '(2 ((shift) . 1)) ;; two lines at a time
        mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
        mouse-wheel-follow-mouse 't)
  :config
  (smooth-scrolling-mode)
  (enable-smooth-scroll-for-function previous-line)
  (enable-smooth-scroll-for-function next-line)
  (enable-smooth-scroll-for-function isearch-repeat))

(use-package dired
  :ensure nil
  :commands dired-jump
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (setq wdired-allow-to-change-permissions t)
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks "@"
        dired-dwim-target t)
  :config
  (bind-key "TAB" 'dired-details-toggle dired-mode-map)
  (bind-key "C-c C-e" 'dired-toggle-read-only)
  (bind-key "C-x C-j" 'dired-jump)
  (bind-key "C-x M-j" '(lambda () (interactive) (dired-jump 1)))
  (bind-key "u" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-<up>" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-p" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
  (bind-key "M-<down>" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map)
  (bind-key "M-n" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map))


(use-package all-the-icons-dired
  :after dired
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; less verbose dired
(use-package dired-details
  :after dired
  :init (setq-default dired-details-hidden-string "")
  :config (dired-details-install))

(use-package syslog-mode
  :mode "\\.log$"
  :init
  (add-hook 'syslog-mode-hook
            (lambda ()
              (bind-key "f" 'syslog-filter-lines evil-normal-state-local-map)
              (bind-key "F" 'hide-lines-show-all evil-normal-state-local-map))))

(use-package fill-column-indicator
  :defer t)

;; Enforce proper whitespace
(use-package ethan-wspace
  :init
  (setq mode-require-final-newline nil)
  (setq require-final-newline nil)
  :config
  (global-ethan-wspace-mode 1))

(use-package highlight-indentation)

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init
  (setq hl-paren-colors '("DeepPink1" "maroon1" "maroon2" "maroon3" "DeepPink3"
                          "DeepPink4" "maroon4" "VioletRed4"
                          "magenta1" "magenta2" "magenta3" "magenta4"
                          "DarkOrchid1" "DarkOrchid3" "DarkOrchid4" "purple4"))
  :config
  (global-highlight-parentheses-mode))

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
  (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
  (t/declare-prefix "h" "Highlight"
                    ;; leader leader clears all highlights
                    "h" 'highlight-symbol
                    "n" 'highlight-symbol-next
                    "N" 'highlight-symbol-prev))

(use-package highlight-numbers
  :init
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package restclient
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

(use-package hackernews
  :commands hackernews
  :defer t)

(use-package helm-insert-line-from-project
  :ensure nil
  :load-path "site-lisp/helm-insert-line-from-project"
  :config
  (t/declare-prefix "c" "Comment/Complete"
                    "l" 't/helm-find-and-insert-line-from-project))

(t/declare-prefix "o" "Other"
                  "C" 'calc-dispatch
                  "d" 'dired
                  "h" 'hackernews
                  "p" 'list-processes
                  "P" 'proced)

(use-package helm-hunks
  :commands helm-hunks
  :init
  (t/declare-prefix "gh" "Hunk"
                    "h" 'helm-hunks))

(use-package spotify
  :ensure nil
  :load-path "site-lisp/spotify/"
  :commands helm-spotify
  :init
  (t/declare-prefix "o" "Other"
                    "s" 'helm-spotify))

(use-package calendar
  :init
  (setq calendar-week-start-day 1
        calendar-date-style 'iso)
  (t/declare-prefix "o" "Other"
                    "c" 'calendar))

(use-package suggest
  :commands suggest
  :init
  (t/declare-prefix "o" "Other"
                    "S" 'suggest))

(t/declare-prefix "o" "Other"
                  "b" 'browse-url
                  "B" 'w3m)

(require 't-shell)
(require 't-org)

(require 't-keys)
(require 't-langs)

;; try doom-modeline
;(require 't-powerline)
(require 'core-modeline)
(setq mode-line-format (doom-modeline))

(t/declare-prefix "o" "Other"
                  "e" 't/eshell)

(t/declare-prefix "E" "Editor"
                  "t" 'load-theme)

(t/declare-prefix "Ep" "Packages"
                  "i" 'package-install
                  "r" 'package-refresh-contents
                  "l" 'paradox-list-packages
                  "U" 't/upgrade-packages)

(t/declare-prefix "Ec" "Edit config"
                  "R" 'config-reload
                  "i" 'config-edit-init
                  "o" 'config-edit-org
                  "d" 'config-edit-sane-defaults
                  "f" 'config-edit-defuns
                  "k" 'config-edit-keys
                  "m" 'config-edit-mac
                  "l" 'config-edit-langs
                  "s" 'config-edit-snippets)

(t/declare-prefix "T" "Toggle"
                  "d" 'toggle-debug-on-error
                  "T" 't/load-theme-cycle
                  "l" 'nlinum-mode
                  "L" 'nlinum-relative-toggle
                  "b" 'fancy-battery-mode
                  "g" 'git-gutter+-mode
                  "c" 'rainbow-mode
                  "r" 'rainbow-delimiters-mode)

(t/declare-prefix "b" "Buffers"
                  "d" 'delete-window
                  "S" 'save-some-buffers
                  "s" 't/switch-to-scratch-buffer
                  "k" 'kill-this-buffer
                  "K" 't/kill-other-buffers
                  "b" 'helm-mini
                  "o" 't/switch-to-previous-buffer
                  "N" 'previous-buffer
                  "n" 'next-buffer
                  "R" 'revert-buffer)

(t/declare-prefix "d" "Doc"
                  "d" 'dash-at-point
                  "s" 'dash-at-point-with-docset
                  "a" 'helm-apropos
                  "f" 'describe-function)

(t/declare-prefix "x" "Text manipulation"
                  "a" 'align-regexp
                  "k" 'ido-kill-buffer
                  "m" 'helm-M-x
                  "x" 'smex-major-mode-commands
                  "ls" 't/sort-lines
                  "lu" 't/uniquify-lines)

(t/declare-prefix "xt" "Transpose"
                  "c" 'transpose-chars
                  "w" 'transpose-words
                  "l" 'transpose-lines
                  "f" 'transpose-frame
                  "s" 'transpose-sexps
                  "S" 'transpose-sentences
                  "p" 'transpose-paragraphs)

(t/declare-prefix "r" "Registers"
                  "r" 'evil-show-registers)

(t/declare-prefix "f" "Files"
                  "f" 'helm-find-files
                  "l" 'neotree-find
                  "J" 'dired-jump
                  "j" 'dired-jump-other-window
                  "g" 'ffap
                  "o" 't/open-in-desktop
                  "r" 'helm-recentf)

(t/declare-prefix "h" "Help"
                  "a" 'helm-apropos
                  "b" 'helm-descbinds
                  "d" 'dash-at-point
                  "f" 'describe-function
                  "k" 'describe-key-briefly
                  "K" 'describe-key
                  "l" 'helm-locate-library
                  "i" 'helm-info-at-point
                  "c" 'describe-char
                  "m" 'describe-mode
                  "M" 'describe-minor-mode
                  "t" 'describe-theme
                  "p" 'describe-package
                  "v" 'describe-variable)

(t/declare-prefix "e" "Errors"
                  "c" 'flycheck-clear
                  "p" 'flycheck-previous-error
                  "n" 'flycheck-next-error
                  "N" 'flycheck-previous-error
                  "l" 'flycheck-list-errors
                  "v" 'flycheck-verify-setup
                  "t" 'flycheck-mode)

(t/declare-prefix "w" "Windows"
                  "n" 'make-frame-command
                  "K" 't/delete-frame-or-hide-last-remaining-frame
                  "k" 'delete-window
                  "t" 'delete-other-windows
                  "o" 't/previous-window
                  "=" 'balance-windows-area
                  "-" 'evil-window-decrease-width
                  "+" 'evil-window-increase-width
                  "u" 'winner-undo
                  "R" 'winner-redo
                  "r" 'transpose-frame)

(t/declare-prefix "z" "Folding"
                  "z" 'hs-toggle-hiding
                  "f" 'hs-hide-block
                  "F" 'hs-hide-all
                  "r" 'hs-show-block
                  "R" 'hs-show-all)

(t/declare-prefix "s" "Search"
                  "s" 'helm-swoop
                  "b" (t/macro-helm-ag-insert 'word helm-ag-buffers)
                  "f" (t/macro-helm-ag-insert 'word helm-ag-this-file)
                  "p" 'helm-projectile-ag
                  "a" 'helm-multi-swoop-all
                  "m" 'helm-multi-swoop
                  "w" (t/macro-helm-ag-insert 'word helm-projectile-ag)
                  "W" (t/macro-helm-ag-insert 'symbol helm-projectile-ag))

(t/declare-prefix "si" "Search Internet"
                  "g" 'helm-google-suggest
                  "w" 'helm-wikipedia-suggest)

(use-package evil-mc)
(use-package evil-mc-extras)

(t/declare-state "mc" "Multiple Cursors state"
                 "+" 'evil-mc-inc-num-at-each-cursor
                 "-" 'evil-mc-dec-num-at-each-cursor
                 "d" '(lambda () (interactive)
                        (when (not evil-mc-mode)
                          (turn-on-evil-mc-mode))
                        (evil-mc-make-all-cursors))
                 "h" '(lambda () (interactive)
                        (when (not evil-mc-mode)
                          (turn-on-evil-mc-mode))
                        (evil-mc-make-cursor-here))
                 "j" #'t/cursor-down
                 "J" #'t/cursor-down-skip
                 "K" #'t/cursor-up-skip
                 "k" #'t/cursor-up
                 "p" 'evil-mc-make-and-goto-prev-cursor
                 "n" 'evil-mc-make-and-goto-next-cursor
                 "q" (lambda () (interactive)
                       (evil-mc-undo-all-cursors)
                       (turn-off-evil-mc-mode)
                       (keyboard-quit)))

(global-set-key [M-S-mouse-1] '(lambda () (interactive)
                                 (when (not evil-mc-mode)
                                   (turn-on-evil-mc-mode))
                                 (evil-mc-make-cursor-here)))

(require 't-load-theme)
(t/load-theme)

;; custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; ;; benchmarks
(when t-debug-init
  (message "t: timing init complete")
  (benchmark-init/show-durations-tabulated)
  (benchmark-init/show-durations-tree))

(require 'server)
(unless (server-running-p) (server-mode))
(put 'narrow-to-page 'disabled nil)
