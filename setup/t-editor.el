;;; -*- lexical-binding: t; -*-
(t/use-package restart-emacs
  :commands restart-emacs
  :init
  (progn
    (t/declare-prefix "q" "Quit"
                      "d" 't/safe-restart-emacs
                      "r" (t/lambda (restart-emacs))
                      "R" (t/lambda (restart-emacs '("--no-desktop"))))))

(t/use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :config (winner-mode))

(t/use-package uniquify ; add dirs to buffer names when not unique
  :ensure nil
  :init
  (progn
    (setq uniquify-buffer-name-style 'forward)))

(t/use-package subword
  :diminish subword-mode
  :defer 1
  :ensure nil
  :config (subword-mode))

(t/use-package tramp
  :defer t
  :ensure nil
  :init
  (t/add-hook-setq 'eshell-mode-hook
                   tramp-default-method "ssh"
                   tramp-auto-save-directory (locate-user-emacs-file ".tramp-auto-save")))

(t/use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :init
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    (setq wdired-allow-to-change-permissions t)
    (setq dired-auto-revert-buffer t
          dired-listing-switches "-alhF"
          dired-ls-F-marks-symlinks "@"
          dired-use-ls-dired nil
          dired-dwim-target t))
  :config
  (progn
    (bind-key "C-x C-j" 'dired-jump)
    (bind-key "C-c C-e" 'dired-toggle-read-only)
    (bind-key "C-x M-j" (t/lambda (dired-jump 1)))
    (evil-define-key 'normal dired-mode-map "u" (t/lambda (find-alternate-file "..")))
    (t/bind-in 'dired-mode-map
      "e" 't/eshell
      "C-d" 'dired-kill-subdir
      "~" (t/lambda (find-alternate-file "~"))
      "M-<up>" (t/lambda (find-alternate-file ".."))
      "M-p" (t/lambda (find-alternate-file ".."))
      "M-<down>" (t/lambda (dired-find-alternate-file))
      "M-n" (t/lambda (dired-find-alternate-file)))))

(t/use-package dired-hacks-utils
  :hook dired-mode-hook
  :ensure nil
  :load-path "site-lisp/dired-hacks-utils/")

(t/use-package dired-avfs
  :hook dired-mode-hook
  :ensure nil
  :load-path "site-lisp/dired-avfs/")

(t/use-package dired-details
  :hook dired-mode-hook
  :ensure nil
  :load-path "site-lisp/dired-details/"
  :init
  (progn
    (setq dired-details-hidden-string "")
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(t/use-package dired-subtree
  :commands dired-subtree-toggle
  :ensure nil
  :load-path "site-lisp/dired-subtree/"
  :init
  (t/after dired
    (setq dired-subtree-line-prefix "  ")
    (bind-key (kbd "<tab>") 'dired-subtree-toggle dired-mode-map)))

(t/use-package all-the-icons-dired
  :commands all-the-icons-dired-mode
  :init
  (t/add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(t/use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode
  :init
  (t/add-hook '(prog-mode-hook css-mode-hook html-mode-hook) 'rainbow-mode))

(t/use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (t/add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(t/use-package neotree
  :commands (neotree-toggle
             neotree-show
             neotree-hide
             neotree-find)
  :init
  (progn
    (setq neo-smart-open nil
          neo-window-fixed-size nil
          neo-window-width 35
          neo-create-file-auto-open t
          neo-mode-line-type 'none
          neo-force-change-root t
          neo-theme 'icons
          neo-show-updir-line nil
          neo-show-hidden-files nil
          neo-auto-indent-point t)
    (t/add-hook-defun 'neotree-mode-hook t/neotree-hook
                      (evil-commentary-mode -1)
                      (evil-extra-operator-mode -1)
                      (hl-line-mode))
    (t/after neotree
      (add-to-list 'window-size-change-functions ;; fixes: https://github.com/jaypei/emacs-neotree/issues/262
                   (lambda (frame)
                     (let ((neo-window (neo-global--get-window)))
                       (unless (null neo-window)
                         (setq neo-window-width (window-width neo-window))))))))

  :config
  (progn
    (bind-key [f6] 'neotree-toggle)
    (dolist (key '(("n" . neotree-next-line)
                   ("p" . neotree-previous-line)
                   ("C-n" . neotree-next-line)
                   ("C-p" . neotree-previous-line)
                   ("c" . neotree-create-node)
                   ("R" . neotree-rename-node)
                   ("D" . neotree-delete-node)
                   ("i" . neotree-enter-horizontal-split)
                   ("C-c o" . neotree-enter-vertical-split)
                   ("s" . neotree-enter-vertical-split)
                   ("g" . neotree-refresh)
                   ("u" . neotree-select-up-node)
                   ("M-<up>" . neotree-select-up-node)
                   ("I" . neotree-hidden-file-toggle)
                   ("q" . neotree-hide)
                   ("q" . neotree-hide)
                   ("v" . neotree-quick-look)
                   ("TAB" . neotree-enter)
                   ("RET" . neotree-enter)
                   ("M-<down>" . neotree-enter)
                   ("C" . neotree-change-root)))
      (eval `(evil-define-key 'normal neotree-mode-map (kbd ,(car key)) ',(cdr key))))))

(t/use-package ace-window
  :commands ace-window)

(t/use-package avy
  :commands (avy-goto-char
             avy-goto-char-2
             avy-goto-line
             avy-goto-char-in-line
             avy-goto-word-0
             avy-goto-line-above
             avy-goto-word-0-above
             avy-goto-word-1-above
             avy-goto-char-2-above
             avy-goto-symbol-1-above
             avy-goto-line-below
             avy-goto-word-0-below
             avy-goto-word-1-below
             avy-goto-char-2-below
             avy-goto-symbol-1-below)
  :init
  (progn
    (setq avy-keys '(?j ?f ?d ?k ?s ?a)
          avy-timeout-seconds 0.2
          avy-all-windows 'all-frames
          avy-case-fold-search nil
          avy-highlight-first t
          avy-style 'at-full
          avy-background t))
  :config
  (progn
    (let ((f 'font-lock-function-name-face))
      (set-face-attribute 'avy-lead-face nil   :background nil :foreground (face-foreground f))
      (set-face-attribute 'avy-lead-face-0 nil :background nil :foreground (face-foreground f))
      (set-face-attribute 'avy-lead-face-1 nil :background nil :foreground (face-foreground f))
      (set-face-attribute 'avy-lead-face-2 nil :background nil :foreground (face-foreground f)))))

(t/use-package es-mode
  :commands es-mode
  :init
  (progn
    (defun t/es-mode-format (status header buffer)
      (with-current-buffer buffer
        (json-pretty-print-buffer)))
    (setq es-response-success-functions '(t/es-mode-format)))
  :config
  (t/bind-in 'es-mode-map
    "C-c C-v" 'es-execute-request-dwim))

(t/use-package hideshow
  :commands evil-toggle-fold
  :ensure nil
  :init
  (progn
    (add-hook 'prog-mode-hook 'hs-minor-mode)
    (defun display-code-line-counts (ov)
      (when (eq 'code (overlay-get ov 'hs))
        (overlay-put ov
                     'display (format " ... "
                                      (count-lines (overlay-start ov)
                                                   (overlay-end ov))))))
    (setq hs-set-up-overlay #'display-code-line-counts)))

(t/use-package ace-jump-mode
  :commands (ace-jump-mode
             ace-jump-char-mode
             ace-jump-line-mode
             ace-jump-word-mode)
  :init
  (progn
    (setq ace-jump-mode-gray-background t
          ace-jump-mode-case-fold t)))

(t/use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-visualize
  :init
  (progn
    (setq undo-tree-auto-save-history nil
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)
    (t/declare-prefix "a" "Applications"
                      "u" 'undo-tree-visualize)))

(t/use-package smex
  :commands (smex smex-major-mode-commands)
  :init
  (progn
    (setq smex-flex-matching t
          smex-save-file (locate-user-emacs-file ".smex-items")))
  :config
  (progn
    (t/bind-in 'global-map
      "C-x C-m" 'smex
      "C-c C-M" 'smex-major-mode-commands)
    (smex-initialize)))

(defun t/company-backends (&optional backends)
  `((,@backends
     company-files
     company-keywords
     company-capf
     company-yasnippet
     company-emoji)
    (company-dabbrev-code
     company-dabbrev
     company-abbrev)))

(t/use-package company
  :diminish company-mode
  :defer 2
  :init
  (progn
    (setq company-idle-delay 0.15
          company-tooltip-align-annotations t
          company-tooltip-flip-when-above nil
          company-show-numbers t ; nav with m-<n>
          company-selection-wrap-around t
          company-tooltip-minimum-width 30
          company-tooltip-margin 1
          company-require-match nil
          company-backends (t/company-backends '()))
    (t/after company
      (global-company-mode)))
  :config
  (progn
    (defun t/company-helm () (interactive) (company-abort) (completion-at-point))
    (t/bind-in 'company-active-map
      "TAB" 'company-complete-selection
      "RET" 'company-complete-selection
      "C-w" 'evil-delete-backward-word
      "C-l" 'evil-delete-backward-word
      "C-u" 'backward-kill-sentence
      "C-n" 'company-select-next
      "C-p" 'company-select-previous
      "C-," #'t/company-helm)))

(t/use-package company-box
  :after company
  :ensure nil
  :load-path "site-lisp/company-box/"
  :init
  (progn
    (setq company-box-doc-delay 0.01)
    (t/add-hook-defun 'company-mode-hook t/company-box-mode-hook
                      (t/after company-box
                        (setq company-box-icons-alist company-box-icons-all-the-icons)
                        (company-box-mode)))))

(t/use-package company-flx
  :after company
  :config
  (progn
    (company-flx-mode +1)))

(t/use-package company-web
  :after company)

(t/use-package company-restclient
  :commands restclient-mode
  :config
  (t/add-company-backends-hook 'restclient-mode-hook 'company-restclient))

(t/use-package helm-xref
  :commands (helm-xref-show-xrefs
             xref-find-definitions
             xref-find-references
             xfref-pop-marker-stack)
  :init
  (progn
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs))
  :config
  (progn
    (t/bind-in 'global-map
      "M-." 'xref-find-definitions
      "M-?" 'xref-find-references
      "M-," 'xref-pop-marker-stack)))

(t/use-package helm-unicode
  :commands helm-unicode)

(t/use-package company-emoji
  :command company-mode
  :after company)

(t/use-package emoji-cheat-sheet-plus
  :commands (emoji-cheat-sheet-plus-insert)
  :init
  (progn
    (t/declare-prefix "a" "applications"
                      "e" 'emoji-cheat-sheet-plus-insert
                      "U" 'helm-unicode))
  :config
  (progn
    ;; make `emoji-cheat-sheet-plus' insert unicodes 🎉
    (defvar t-emoji-cheat-sheet-plus-use-unicode t)

    (defun t/emoji-cheat-shet-plus--unicode-for-emoji-text (text)
      (let* ((emojis (company-emoji-list-create))
             (ret (-first
                   (lambda (emoji)
                     (let ((emoji-text (t/strip-text-properties emoji)))
                       (equal emoji-text text)))
                   emojis)))
        (when ret
          (get-text-property 0 :unicode ret))))

    (defun emoji-cheat-sheet-plus--insert-selection (_)
      "Override to insert the selected emojis into the buffer."
      (let ((emojis (company-emoji-list-create)))
        (dolist (c (helm-marked-candidates))
          (save-match-data
            (string-match "\:.+?\:" c)
            (let ((emoji (match-string 0 c)))
              (insert
               (if t-emoji-cheat-sheet-plus-use-unicode
                   (t/emoji-cheat-shet-plus--unicode-for-emoji-text emoji)
                 emoji)))))))))

(t/use-package smartparens
  :diminish smartparens-mode
  :commands turn-on-smartparens-mode
  :init
  (progn
    (t/after smartparens
      (setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list)))

    (sp-use-paredit-bindings)

    ;; TODO torgeir
    ;; interfers with e.g. org-mode, enable them specifically in lisp modes instead
    (unbind-key "M-?" sp-keymap)
    (comment (unbind-key "M-<up>" sp-keymap)
             (unbind-key "M-<down>" sp-keymap)
             (unbind-key "C-<right>" sp-keymap)
             (unbind-key "C-<left>" sp-keymap))

    (t/bind-in 'sp-keymap
      ;; sp bindings
      "C-M-f" 'sp-forward-sexp
      "C-M-b" 'sp-backward-sexp
      "C-M-d" 'sp-down-sexp
      "C-M-S-d" 'sp-backward-down-sexp
      "C-M-a" 'sp-beginning-of-sexp
      "C-M-e" 'sp-end-of-sexp
      "C-M-S-e" 'sp-up-sexp
      "C-M-u" 'sp-backward-up-sexp
      "C-M-n" 'sp-next-sexp
      "C-M-p" 'sp-previous-sexp
      "C-M-k" 'sp-kill-sexp
      "C-M-w" 'sp-copy-sexp

      ;; paredit bindings
      "<delete>" 'sp-delete-char
      "<backspace>" 'sp-backward-delete-char
      "C-<right>" #'sp-forward-slurp-sexp
      "C-<left>" #'sp-forward-barf-sexp
      "M-<up>" 'sp-splice-sexp-killing-backward
      "M-<down>" 'sp-splice-sexp-killing-forward

      ;; extras
      "M-S-<up>" #'sp-backward-up-sexp
      "M-S-<down>" #'sp-down-sexp
      "M-S-<left>" #'sp-backward-sexp
      "M-S-<right>" #'sp-forward-sexp)

    (t/bind-in 'global-map
      "s-(" 't/wrap-with-parens
      "s-)" 't/paredit-wrap-round-from-behind
      "M-s-(" 't/wrap-with-braces
      "M-s-[" 't/wrap-with-brackets)

    (bind-key "RET" #'t/newline-expand-braces)

    (t/add-hook '(js-mode-hook
                  text-mode-hook
                  restclient-mode-hook
                  rjsx-mode
                  ruby-mode
                  mark-down-mode
                  es-mode-hook) 'turn-on-smartparens-mode)

    ;; enable in minibuffer
    (t/add-hook 'eval-expression-minibuffer-setup-hook #'(turn-on-smartparens-mode evil-cleverparens-mode))

    (defun t/enable-movement-for-lisp-mode (m)
      (lexical-let* ((mode (symbol-name m))
                     (mode-hook (intern (concat mode "-hook")))
                     (mode-map (intern (concat mode "-map"))))
        (add-hook mode-hook 'turn-on-smartparens-mode)
        (add-hook mode-hook 'evil-cleverparens-mode)))

    (t/after elisp-mode (t/enable-movement-for-lisp-mode 'emacs-lisp-mode))
    (t/after ielm (t/enable-movement-for-lisp-mode 'ielm-mode))
    (t/after clojure-mode (t/enable-movement-for-lisp-mode 'clojure-mode))

    (dolist (mode '(emacs-lisp-mode clojure-mode ielm-mode minibuffer-inactive-mode))
      (sp-local-pair mode "`" nil :actions nil)
      (sp-local-pair mode "'" nil :actions nil))

    (sp-with-modes 'emacs-lisp-mode
      (sp-local-pair "`" "'" :when '(sp-in-docstring-p)))

    (t/def-pairs ((paren . "(")
                  (bracket . "[")
                  (brace . "{")
                  (single-quote . "'")
                  (double-quote . "\"")
                  (back-quote . "`"))))
  :config
  (progn
    (t/bind-in 'text-mode-map
      "C-<right>" 'sp-forward-slurp-sexp
      "C-<left>" 'sp-forward-barf-sexp)))

(use-package csv-mode
  :commands csv-mode
  :init
  (setq csv-separators '(";")))

(t/use-package writeroom-mode
  :commands writeroom-mode
  :init
  (setq writeroom-width 0.66
        writeroom-maximize-window nil
        writeroom-fullscreen-effect 'maximized
        writeroom-major-modes '(js-mode js2-mode rjsx-mode)))

(t/use-package w3m
  :commands w3m
  :config
  (t/bind-in 'w3m-mode-map
    "M-p" 'backward-paragraph
    "M-n" 'forward-paragraph))

(t/use-package discover-my-major
  :commands (discover-my-major discover-my-mode))

(t/use-package helm
  :commands (completion-at-point
             helm
             helm-mini
             helm-projectile
             helm-projectile-ag)
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (t/after helm
      (t/bind-in 'helm-map
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-c u" 'universal-argument))
    (t/after helm-command
      (t/bind-in 'helm-M-x-map
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-c u" 'universal-argument))
    (t/after helm-files
      (t/bind-in 'helm-find-files-map
        "M-<down>" 'helm-execute-persistent-action
        "M-<up>" 'helm-find-files-up-one-level
        "C-k" 'helm-find-files-up-one-level))
    (setq-default helm-candidate-number nil
                  helm-display-header-line nil
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
                  helm-follow-mode-persistent t ; keep follow mode on, after on once
                  helm-ff-skip-boring-files t
                  helm-quick-update t
                  helm-M-x-requires-pattern nil
                  helm-show-completion-display-function #'helm-show-completion-default-display-function)

    (require 'helm-mode)
    (helm-mode 1)
    (t/add-hook-defun 'helm-after-initialize-hook t/hide-cursor-in-helm-buffer
                      (with-helm-buffer
                        (setq cursor-in-non-selected-windows nil)))
    (set-face-attribute 'helm-source-header nil :height 1)
    (t/after neotree
      (t/add-hook 'helm-before-initialize-hook 'neotree-hide))))

(t/use-package helm-ag
  :after helm
  :commands (helm-ag helm-projectile-ag)
  :init
  (progn
    (setq helm-ag-fuzzy-match t
          helm-ag-insert-at-point 'symbol
          helm-ag-use-grep-ignore-list t
          ;; save edited buffers on completion
          helm-ag-edit-save t)
    (when is-ms
      (setq helm-ag-base-command "ag --nocolor --nogroup --vimgrep"))))

(t/use-package helm-projectile
  :after helm
  :commands (helm-projectile projectile-load-known-projects helm-projectile-ag))

(t/use-package helm-descbinds
  :commands helm-descbinds
  :config
  (progn
    (helm-descbinds-mode)
    (setq helm-descbinds-window-style 'split)))

(t/use-package helm-google
  :commands helm-google)

(t/use-package swiper-helm
  :commands swiper-helm)

(t/use-package visual-regexp
  :commands vr/query-replace
  :config
  (progn
    (bind-key "M-%" 'vr/query-replace)))

(t/use-package dash-at-point
  :commands dash-at-point)

(t/use-package expand-region
  :commands (er/expand-region er/contract-region)
  :init
  (progn
    (bind-key (if is-mac "M-@" "M-'") 'er/expand-region)
    (bind-key (if is-mac "M-*" "M-§") 'er/contract-region)))

(t/use-package transpose-frame
  :commands transpose-frame)

(t/use-package yasnippet
  :diminish yas-minor-mode
  :defer 1
  :init
  (progn
    (setq yas-snippet-dirs '(t-dir-snippets)
          ;; remove dropdowns
          ;;yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
          yas-verbosity 0
          yas-wrap-around-region t))
  :config
  (progn
    (yas-global-mode)

    (defun t/yas-clear-or-delete-char ()
      "Replace `yas-next-field' with noop `+' to make <backspace> only clear or delete-char."
      (interactive)
      (cl-letf (((symbol-function 'yas-next-field) #'+))
        (call-interactively 'yas-skip-and-clear-or-delete-char)))
    (bind-key "<backspace>" #'t/yas-clear-or-delete-char yas-keymap)

    (t/add-hook-defun 'after-save-hook t/reload-autoloads-on-defuns-save
                      (when (string-match "\\t-defuns.el$" buffer-file-name)
                        (eval-buffer)
                        (t/reload-autoloads)
                        (message "Reloaded autoloads.")))
    (t/add-hook-defun 'after-save-hook t/reload-snippets-on-save
                      (t/when-ext "yasnippet" (yas-reload-all)))

    ;; make fundamental snippets global snippets
    (t/add-hook-defun 'yas-minor-mode-hook t/hook-yas (yas-activate-extra-mode 'fundamental-mode))

    ;; jump to end of snippet definition
    (bind-key "<return>" 'yas-exit-all-snippets yas-keymap)

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

    (t/bind-in 'yas-keymap
      "C-e" 'yas/goto-end-of-active-field
      "C-a" 'yas/goto-start-of-active-field)))

(t/use-package ag
  :commands ag
  :config
  (progn
    (setq ag-reuse-buffers t
          ag-reuse-window t
          ag-highlight-search t
          ag-project-root-function (lambda (d) (projectile-project-root)))))

(t/use-package wgrep :after ag)
(t/use-package wgrep-ag :after ag)

(t/use-package autorevert
  :ensure nil
  :defer t
  :init
  (progn
    (setq auto-revert-interval 1
          ;; silenced refresh of dired
          auto-revert-verbose nil
          global-auto-revert-non-file-buffers t))
  :config
  (progn
    (global-auto-revert-mode)
    (when is-mac
      ;; file notifications aren't supported on os x
      (setq auto-revert-use-notify nil))))

(t/use-package smooth-scrolling
  :commands (previous-line next-line isearch-repeat)
  :init
  (progn
    (setq smooth-scroll-margin 4
          mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
          mouse-wheel-follow-mouse 't))
  :config
  (progn
    (smooth-scrolling-mode)
    (enable-smooth-scroll-for-function previous-line)
    (enable-smooth-scroll-for-function next-line)
    (enable-smooth-scroll-for-function isearch-repeat)))

(t/use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :defer 1
  :init
  (progn
    (setq hl-paren-colors (-repeat 26 "DeepPink"))
    (t/add-hook-defun 'prog-mode-hook t-hook-l-parens
                      (highlight-parentheses-mode)
                      (set-face-foreground 'show-paren-match "Green"))))

(t/use-package highlight-escape-sequences
  ;; what the 
  :hook (prog-mode-hook hes-mode)
  :config
  (progn
    (put 'hes-escape-backslash-face 'face-alias 'font-lock-comment-face)
    (put 'hes-escape-sequence-face 'face-alias 'font-lock-comment-face)))

(t/use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands (highlight-symbol-mode
             highlight-symbol
             highlight-symbol-next
             highlight-symbol-prev)
  :init
  (progn
    (setq highlight-symbol-idle-delay 0.5)
    (t/add-hook 'prog-mode-hook 'highlight-symbol-mode))
  :config
  ;; highlight-symbol uses hl-line-face
  (require 'hl-line))

(t/use-package highlight-numbers
  :defer 1
  :init
  (progn
    (t/add-hook 'prog-mode-hook 'highlight-numbers-mode)))

(use-package js-codemod
  ;; :ensure nil
  ;; :load-path "~/Desktop/js-codemod/js-codemod.el"
  :commands (js-codemod-mod-region))

(use-package helm-js-codemod
  ;;:ensure nil
  ;;:load-path "~/Desktop/js-codemod/helm-js-codemod.el"
  :commands (helm-js-codemod)
  :init
  (setq helm-js-codemod-mod-dir
        (expand-file-name "~/Desktop/js-codemod/mods/")))

(t/use-package restclient
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

;; eww-mode
(defun t/eww-readable-after-render (status url buffer)
  (eww-render status url nil buffer)
  (switch-to-buffer buffer)
  (eww-readable)
  (writeroom-mode 1))

(defun t/eww-readable (url)
  (interactive "sEnter URL: ")
  (let ((buffer (get-buffer-create "*eww*")))
    (with-current-buffer buffer
      (autoload 'eww-setup-buffer "eww")
      (eww-setup-buffer)
      (url-retrieve url 't/eww-readable-after-render (list url buffer)))))

(t/after shr
  ;; don't truncate lines in eww-mode
  ;;(setq shr-width nil)
  (defun shr-fill-text (text) text)
  (defun shr-fill-lines (start end) nil)
  (defun shr-fill-line () nil)

  ;; wrap lines
  (t/add-hook-defun 'eww-after-render-hook t/hook-eww-trunc
                    (toggle-truncate-lines -1)))

;; not to large images
(setq shr-use-fonts nil
      shr-max-image-proportion 0.6
      shr-ignore-cache t)

(defun t/visit-frontmost-chrome-url-in-eww ()
  "Visit the front-most url of chrome in eww."
  (interactive)
  (eww (t/grab-chrome-url)))

(t/after eww
  (t/add-hook-defun 'eww-mode-hook t/hook-eww
                    (t/declare-prefix-for-mode 'eww-mode
                                               "t" "Toggle"
                                               "i" 't/eww-toggle-images)
                    (t/bind-in '(evil-normal-state-local-map)
                      "q" 'quit-window
                      "S-TAB" 'shr-previous-link
                      "TAB" 'shr-next-link
                      "R" 'eww-readable
                      "M-p" 'backward-paragraph
                      "M-n" 'forward-paragraph
                      "s-l" 'eww)
                    (visual-line-mode)))

(t/use-package hackernews
  :commands hackernews
  :init
  (defun t/hackernews ()
    "Open hackernews in current window."
    (interactive)
    (cl-letf (((symbol-function 'pop-to-buffer) #'switch-to-buffer))
      (call-interactively 'hackernews)))
  :config
  (progn
    (evil-define-key 'normal hackernews-map
      (kbd "<return>") 'hackernews-button-browse-internal
      (kbd "TAB") 'hackernews-next-comment
      "q" 'quit-window
      "j" 'hackernews-next-item
      "k" 'hackernews-previous-item
      "gr" 'hackernews-load-more-stories
      "gR" 'hackernews-reload)))

(t/use-package helm-hunks
  :commands (helm-hunks
             helm-hunks-current-buffer
             helm-hunks-staged
             helm-hunks-staged-current-buffer)
  :init
  (progn
    (setq helm-hunks-preview-diffs t)
    (t/declare-prefix "gh" "Hunk"
                      "h" 'helm-hunks
                      "H" 'helm-hunks-current-buffer
                      "SS" 'helm-hunks-staged
                      "SH" 'helm-hunks-staged-current-buffer)))

(t/use-package calendar
  :ensure nil
  :commands calendar
  :init
  (progn
    (setq calendar-week-start-day 1
          calendar-date-style 'iso)))

(t/use-package suggest
  :commands suggest)

;; save more recent files
(t/use-package recentf
  :ensure nil
  :defer 1
  :init
  (progn
    (setq recentf-max-saved-items 1000
          recentf-auto-cleanup 'never))
  :config
  (progn
    (defun t/recentf-save-if-recentf-mode ()
      (when recentf-mode (recentf-save-list)))
    (t/idle-timer recentf-auto-save-timer #'t/recentf-save-if-recentf-mode 1)
    (recentf-mode 1)))

(t/use-package nlinum
  :commands nlinum-mode
  :init
  (setq nlinum-format " %d "))

(t/use-package nlinum-relative
  :commands nlinum-relative-toggle
  :init
  (setq nlinum-relative-redisplay-delay 0))

(t/use-package projectile
  :diminish projectile-mode
  :commands (projectile-mode
             helm-projectile
             projectile-project-root
             projectile-relevant-known-projects
             projectile-load-known-projects)
  :init
  (progn
    (setq projectile-completion-system 'helm
          projectile-require-project-root nil
          projectile-known-projects-file (locate-user-emacs-file ".cache/projectile.projects")
          projectile-cache-file (locate-user-emacs-file ".cache/projectile.cache")
          projectile-enable-caching t
          projectile-project-root-files '(".git" ".hg" ".svn" ".project" "package.json" "setup.py" "Gemfile" "build.gradle")))
  :config
  (progn
    (t/add-to-list 'projectile-globally-ignored-directories '("elpa-backups" "node_modules" "target" "dist" ".idea"))
    (t/add-to-list 'projectile-globally-ignored-files '("package-lock.json" "**.bundle.js" "**.build.js" ".DS_Store" "projectile.cache" "custom.el"))
    (t/add-to-list 'grep-find-ignored-files '("package-lock.json" "**.bundle.js" "**.build.js" ".DS_Store" "custom.el" "node_modules/**"))
    (projectile-global-mode +1)))

(t/use-package dumb-jump
  :commands dumb-jump-go
  :init
  (progn
    (setq dumb-jump-selector 'helm)
    (t/bind-in '(evil-normal-state-map evil-insert-state-map)
      "M-." 'dumb-jump-go
      "M-." 'dumb-jump-go)

    (t/add-hook-defun 'emacs-lisp-mode-hook t/hook-elisp
                      (bind-key "M-." 'xref-find-definitions evil-normal-state-map)
                      (bind-key "M-." 'xref-find-definitions evil-insert-state-map))))

(t/use-package aggressive-indent
  :commands (aggressive-indent-mode global-aggressive-indent-mode)
  :init
  (progn
    (t/add-hook-defun 'json-mode-hook t/hook-aggressive-indent-json (aggressive-indent-mode 0))
    (t/add-hook-defun 'js-mode-hook t/hook-aggressive-indent-js (aggressive-indent-mode 0))
    (t/add-hook-defun 'elm-mode-hook t/hook-aggressive-indent-js (aggressive-indent-mode 0))
    (t/add-hook-defun 'js2-mode-hook t/hook-aggressive-indent-js2 (aggressive-indent-mode 0))
    (t/add-hook-defun 'css-mode-hook t/hook-aggressive-indent-css (aggressive-indent-mode 0))
    (t/add-hook 'prog-mode-hook 'aggressive-indent-mode)
    (t/declare-prefix "t" "Toggle"
                      "a" 'aggressive-indent-mode)))

(use-package whitespace
  :ensure nil
  :init
  (progn
    (t/add-hook 'before-save-hook 'whitespace-cleanup)
    (t/add-hook-defun '(prog-mode-hook text-mode-hook git-commit-mode-hook) t/hook-whitespace
                      (setq-local whitespace-style '(face tabs tab-mark trailing))
                      (whitespace-mode 1))))

(use-package doc-view
  :defer t
  :ensure nil
  :init
  (setq doc-view-continuous t)
  :config
  (t/after evil
    (evil-set-initial-state 'doc-view-mode 'normal)
    (evil-make-overriding-map doc-view-mode-map 'normal)
    (evil-define-key 'normal doc-view-mode-map
      "gg" 'doc-view-first-page
      "G" 'doc-view-last-page
      "H" 'doc-view-fit-height-to-window
      "W" 'doc-view-fit-width-to-window
      "+" 'doc-view-enlarge
      "-" 'doc-view-shrink
      "/" (t/lambda () (let ((current-prefix-arg 4)) (call-interactively 'doc-view-search)))
      "?" (t/lambda () (let ((current-prefix-arg 4)) (call-interactively 'doc-view-search-backward)))
      "n" 'doc-view-search-next-match
      "p" 'doc-view-search-previous-match
      "j" 'doc-view-next-line-or-next-page
      "k" 'doc-view-previous-line-or-previous-page
      "q" (t/lambda () (doc-view-kill-proc) (quit-window)))
    (t/bind-in 'doc-view-mode-map
      "C-u" 'doc-view-scroll-down-or-previous-page
      "C-d" 'doc-view-scroll-up-or-next-page)))


(use-package artist-mode
  :ensure nil
  :defer t
  :commands (t/artist-mode artist-mode)
  :init
  (progn
    (defun t/artist-mode ()
      (interactive)
      (if (and (boundp 'artist-mode)
               artist-mode)
          (progn
            (artist-mode-off)
            (evil-normal-state))
        (progn
          (switch-to-buffer "*scratch*")
          (aggressive-indent-mode 0)
          (evil-insert-state)
          (artist-mode t))))

    (t/add-hook-defun 'artist-mode-hook t/hook-artist
                      (t/bind-in '(evil-normal-state-local-map evil-insert-state-local-map)
                        "q" 'artist-mode-off))

    (t/after evil-leader
      (t/declare-prefix "aa" "drawing"
                        "t" #'t/artist-mode
                        "p" 'artist-select-op-pen-line
                        "l" 'artist-select-op-line
                        "r" 'artist-select-op-rectangle
                        "c" 'artist-select-op-circle
                        "e" 'artist-select-op-ellipse
                        "s" 'artist-select-op-square))))

(t/use-package try
  :commands try)

(t/use-package dictionary
  :commands (dictionary dictionary-search)
  :init
  (t/declare-prefix "sd" "dictionary"
                    "d" (t/lambda (dictionary-search (t/word-at-point)))))

(t/use-package synosaurus
  :commands synosaurus-lookup
  :init
  (progn
    (setq synosaurus-choose-method 'popup
          synosaurus-backend 'synosaurus-backend-wordnet)
    (t/declare-prefix "sd" "dictionary"
                      "s" (t/lambda (synosaurus-lookup (t/word-at-point))))))

(t/use-package selectric-mode ; lol
  :commands selectric-mode)

(defun t-editor/config ()

  (t/after re-builder (setq reb-re-syntax 'rx))
  (t/add-hook 'text-mode-hook 'auto-fill-mode) ; wrap text in text modes
  (t/add-hook 'text-mode-hook 'goto-address-prog-mode) ; navigate urls
  (t/add-hook 'focus-out-hook #'garbage-collect) ; make it feel snappier
  (t/add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
  (t/add-hook 'find-file 't/find-file-check-make-large-file-read-only-hook)
  (setq large-file-warning-threshold (* 20 ; mb
                                        1024 1024))

  (progn
    ;; inline evaled results when in elisp using cider
    (autoload 'cider--make-result-overlay "cider-overlays")
    (defun endless/eval-overlay (value point)
      (cider--make-result-overlay (format "%S" value) :where point :duration 'command) value) ; preserve the return value
    (advice-add 'eval-region :around (lambda (f beg end &rest r) (endless/eval-overlay (apply f beg end r) end)))
    (advice-add 'eval-last-sexp :filter-return (lambda (r) (endless/eval-overlay r (point))))
    (advice-add 'eval-defun :filter-return (lambda (r) (endless/eval-overlay r (save-excursion (end-of-defun) (point))))))

  (evil-leader/set-key "'" 't/eshell)
  (evil-leader/set-key "<" 't/eshell)
  (evil-leader/set-key "|" 't/eshell)
  (evil-leader/set-key "TAB" 't/switch-to-previous-buffer)
  (evil-leader/set-key "u" 'universal-argument)

  (defvar t-regex-mode nil "reb-mode on or not")
  (defun t/toggle-regex-mode ()
    (interactive)
    (if t-regex-mode (reb-quit) (re-builder))
    (setq t-regex-mode (not t-regex-mode)))

  (t/declare-prefix "a" "Applications"
                    "B" 'w3m
                    "c" 'calendar
                    "b" #'t/browse-url-at-point
                    "C" 'calc-dispatch
                    "h" 't/hackernews
                    "i" 't/open-in-intellij
                    "p" 'proced
                    "m" 'helm-spotify
                    "n" (t/lambda (t/eww-readable "https://www.nrk.no/nyheter/"))
                    "R" #'t/toggle-regex-mode
                    "se" 't/eshell
                    "st" 'ansi-term
                    "sT" 'term
                    "ss" 'shell
                    "S" 'suggest
                    "w" 'eww
                    "W" (t/lambda nil
                          (t/eww-readable "https://en.wikipedia.org/wiki/Special:Random")
                          (visual-line-mode -1)
                          (visual-line-mode 1)))

  (t/declare-prefix "fe" "Editor")

  (t/declare-prefix "fe" "Files"
                    "R" 't/config-reload
                    "i" 't/helm-files-emacs-init-files)

  (t/declare-prefix "fep" "Packages"
                    "i" 'package-install
                    "r" 'package-refresh-contents
                    "l" 'paradox-list-packages
                    "R" 'package-reinstall
                    "U" 't/upgrade-packages)

  (t/declare-prefix "t" "Toggle"
                    "d" 'toggle-debug-on-error
                    "f" 't/cycle-font
                    "F" 'toggle-frame-fullscreen
                    "t" 't/load-theme-cycle
                    "m" 'menu-bar-mode
                    "n" #'t/toggle-line-numbers
                    "r" #'t/toggle-relative-line-numbers
                    "L" 'hl-line-mode
                    "l" 'visual-line-mode
                    "W" 'whitespace-mode
                    "w" 'writeroom-mode
                    "Cc" 'rainbow-mode
                    "Cd" 'rainbow-delimiters-mode)

  (t/declare-prefix "b" "Buffers"
                    "S" 'save-some-buffers
                    "s" 't/switch-to-scratch-buffer
                    "d" 'kill-this-buffer
                    "t" 't/kill-other-buffers
                    "b" 'helm-mini
                    "o" 't/switch-to-previous-buffer
                    "N" 'previous-buffer
                    "p" 'previous-buffer
                    "n" 'next-buffer
                    "R" 'revert-buffer)

  (t/declare-prefix "d" "Doc/Desktop"
                    "d" 'dash-at-point
                    "S" 'dash-at-point-with-docset
                    "s" #'t/desktop-save
                    "r" #'t/desktop-restore
                    "c" 'desktop-clear)

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

  (t/declare-prefix "f" "Files/Frame"
                    "b" 'helm-bookmarks
                    "f" 'helm-find-files
                    "l" 't/neotree-open-file
                    "L" 'neotree-hide
                    "j" 'dired-jump
                    "J" 'dired-jump-other-window
                    "d" 'delete-frame
                    "g" 'ffap
                    "o" 't/open-in-desktop
                    "r" 'helm-recentf)

  (t/declare-prefix "h" "Help"
                    "h" #'t/describe
                    "f" #'t/face-at-point
                    "a" 'helm-apropos
                    "l" 'helm-locate-library
                    "i" 'helm-info-at-point
                    "I" 'helm-info
                    "r" 'helm-resume
                    "db" 'helm-descbinds
                    "dd" 'dash-at-point
                    "df" 'describe-function
                    "dk" 'describe-key-briefly
                    "dK" 'describe-key
                    "dc" 'describe-char
                    "dm" 'describe-mode
                    "dM" 'describe-minor-mode
                    "dt" 'describe-theme
                    "dp" 'describe-package
                    "dv" 'describe-variable)

  (t/declare-prefix "e" "Errors"
                    "c" 'flycheck-clear
                    "p" 'flycheck-previous-error
                    "n" 'flycheck-next-error
                    "N" 'flycheck-previous-error
                    "l" 'flycheck-list-errors
                    "v" 'flycheck-verify-setup
                    "t" 'flycheck-mode)

  (t/declare-prefix "w" "Windows"
                    "h" 'windmove-left
                    "j" 'windmove-down
                    "k" 'windmove-up
                    "l" 'windmove-right
                    "n" 'make-frame-command
                    "D" 't/delete-frame-or-hide-last-remaining-frame
                    "d" 'delete-window
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

  (t/declare-prefix "j" "Jump to"
                    "w" 'ace-window
                    "j" 'avy-goto-char-timer
                    "t" 'avy-goto-char-timer
                    "c" 'avy-goto-char
                    "C" 'avy-goto-char-2
                    "l" 'avy-goto-line
                    "L" 'avy-goto-char-in-line
                    "W" 'avy-goto-word-1)

  (t/declare-prefix "ja" "Jump to above"
                    "l" 'avy-goto-line-above
                    "W" 'avy-goto-word-0-above
                    "w" 'avy-goto-word-1-above
                    "c" 'avy-goto-char-2-above
                    "s" 'avy-goto-symbol-1-above)

  (t/declare-prefix "jb" "Jump to below"
                    "l" 'avy-goto-line-below
                    "w" 'avy-goto-word-1-below
                    "W" 'avy-goto-word-0-below
                    "c" 'avy-goto-char-2-below
                    "s" 'avy-goto-symbol-1-below)

  (t/declare-prefix "c" "Comment/Complete"
                    "l" 'helm-lines)

  (t/declare-prefix "h" "Highlight"
                    "H" 'highlight-symbol
                    "n" 'highlight-symbol-next
                    "N" 'highlight-symbol-prev)

  (t/declare-prefix "p" "Project"
                    "b" 'helm-browse-project
                    "c" 'projectile-switch-project
                    "d" 't/projectile-dired
                    "w" #'t/projectile-desktop
                    "f" 'helm-projectile
                    "F" 'projectile-find-file-dwim
                    "g" 't/projectile-magit-status
                    "G" 'projectile-regenerate-tags
                    "k" 'projectile-kill-buffers
                    "l" 't/neotree-open-file
                    "L" 'neotree-hide
                    "o" 't/open-in-desktop
                    "p" 't/projectile-visit-git-link-pulls
                    "s" 't/projectile-helm-ag
                    "R" 'projectile-replace
                    "S" 'projectile-save-project-buffers
                    "t" 'projectile-find-test-file)

  (t/declare-prefix "s" "Search"
                    "I" 'helm-imenu-in-all-buffers
                    "T" 'helm-etags-select
                    "a" 'helm-multi-swoop-all
                    "b" (t/macro-helm-ag-insert 'word helm-ag-buffers)
                    "f" (t/macro-helm-ag-insert 'word helm-ag-this-file)
                    "g" 'helm-google
                    "G" 'helm-google-suggest
                    "i" 'helm-imenu
                    "m" 'helm-multi-swoop
                    "p" 'helm-projectile-ag
                    "s" 'swiper-helm
                    "t" 'etags-select-find-tag-at-point
                    "w" 'helm-wikipedia-suggest))

(provide 't-editor)
