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

(use-package dash)
(use-package s)
(use-package better-defaults)

(require 'sane-defaults)
(require 'defuns)
(require 'setup-evil)

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
  (setq which-key-idle-delay 0.8)
  (which-key-mode 0)
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
  (evil-leader/set-key
    "t"  'projectile-find-file-dwim
    "pc" 'projectile-switch-project
    "pg" 'helm-projectile-ag
    "pf" 'projectile-find-file
    "pd" 'projectile-find-dir
    "pt" 'projectile-find-test-file
    "pk" 'projectile-kill-buffers
    "po" 'open-in-desktop))

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
            '(lambda ()
               (defun neotree-change-root-up ()
                 (interactive)
                 (neotree-select-up-node)
                 (neotree-select-up-node))
               (bind-key "C-n" 'neotree-next-line evil-normal-state-local-map)
               (bind-key "C-p" 'neotree-previous-line evil-normal-state-local-map)
               (bind-key "u" 'neotree-change-root-up evil-normal-state-local-map)
               (bind-key "q" 'neotree-hide evil-normal-state-local-map)
               (bind-key "RET" 'neotree-enter evil-normal-state-local-map)
               (bind-key "C" 'neotree-change-root evil-normal-state-map))))

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (setq git-gutter+-modified-sign "  ")
  (setq git-gutter+-added-sign "+ ")
  (setq git-gutter+-deleted-sign "- ")
  (global-git-gutter+-mode t)
  (evil-leader/set-key
    "ghn" 'git-gutter+-next-hunk
    "ghN" 'git-gutter+-previous-hunk
    "ghC" 'git-gutter+-stage-and-commit
    "gh=" 'git-gutter+-show-hunk
    "ghr" 'git-gutter+-revert-hunks
    "ghs" 'git-gutter+-stage-hunks
    "ghc" 'git-gutter+-commit))

(use-package git-timemachine
  :commands git-timemachine
  :defer t
  :init
  (evil-leader/set-key "vt" 'git-timemachine-toggle))

(use-package gist
  :init
  (evil-leader/set-key
    "ggl" 'gist-list
    "ggb" 'gist-buffer
    "ggB" 'gist-buffer-private
    "ggr" 'gist-region
    "ggR" 'gist-region-private))

(use-package ace-jump-mode
  :init
  (setq ace-jump-mode-gray-background nil
        ace-jump-mode-case-fold t)
  (evil-leader/set-key
    "jj" 'ace-jump-mode
    "jc" 'ace-jump-char-mode
    "jl" 'ace-jump-line-mode
    "jw" 'ace-jump-word-mode))


(use-package magit
  :commands magit-status
  :defer t
  :init
  (evil-leader/set-key
    "gs" 'magit-status
    "gb" 'magit-blame-mode
    "gl" 'magit-log
    "gC" 'magit-commit)
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
    (add-hook hook 'turn-on-smartparens-mode)))

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

(use-package helm
  :diminish helm-mode
  :bind (("C-x b" . helm-buffers-list)
         ("C-x C-m" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring))
  :init
  (require 'helm-config)
  (setq-default helm-display-header-line nil
                helm-M-x-fuzzy-match t
                helm-buffers-fuzzy-matching t
                helm-recentf-fuzzy-match t
                helm-apropos-fuzzy-match t
                helm-projectile-fuzzy-match t
                helm-completion-in-region-fuzzy-match t

                helm-ff-skip-boring-files t
                helm-quick-update t
                helm-M-x-requires-pattern nil)
  (helm-mode)
  (set-face-attribute 'helm-source-header nil :height 1)
  :config
  (progn

    (bind-key "C-w" 'backward-kill-word helm-map)

    (use-package helm-ag)

    (use-package helm-projectile)

    (use-package helm-descbinds
      :init
      (helm-descbinds-mode))))

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
  (evil-leader/set-key
    "hc" 'highlight-symbol-remove-all
    "hh" 'highlight-symbol-at-point
    "hn" 'highlight-symbol-next
    "hN" 'highlight-symbol-prev))

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
  (global-set-key (kbd "C-c g c") 'geeknote-create)
  (global-set-key (kbd "C-c g e") 'geeknote-edit)
  (global-set-key (kbd "C-c g f") 'geeknote-find)
  (global-set-key (kbd "C-c g s") 'geeknote-show)
  (global-set-key (kbd "C-c g r") 'geeknote-remove)
  (global-set-key (kbd "C-c g m") 'geeknote-move)

  (evil-leader/set-key
    "gnc" 'geeknote-create
    "gne" 'geeknote-edit
    "gnf" 'geeknote-find
    "gns" 'geeknote-show
    "gnr" 'geeknote-remove
    "gnm" 'geeknote-move))

(use-package spotify
  :ensure nil
  :load-path "site-lisp/spotify/"
  :commands helm-spotify
  :init
  (evil-leader/set-key
    "os" 'helm-spotify))

;; minibuffer
(bind-key "C-w" 'backward-kill-word minibuffer-local-map)

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
(bind-key "C-x C-c" 'delete-frame)
(eval-after-load "evil"
  '(progn
     (defadvice evil-quit (around advice-for-evil-quit activate) (message "really?"))
     (defadvice evil-quit-all (around advice-for-evil-quit-all activate) (message "really?"))))

(use-package spacemacs-theme
  :init (load-theme 'spacemacs-dark t))
