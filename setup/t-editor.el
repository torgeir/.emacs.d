;;; -*- lexical-binding: t; -*-
(t/use-package restart-emacs
  :commands restart-emacs
  :init
  (progn
    (t/declare-prefix "q" "Quit"
                      "r" (lambda () (interactive) (restart-emacs))
                      "R" (lambda () (interactive) (restart-emacs '("--no-desktop"))))))

(t/use-package winner
  :only-standalone t
  :ensure nil
  :commands (winner-undo winner-redo)
  :config
  (progn
    (winner-mode 1)
    (bind-key "C-c <left>" 'winner-undo)
    (bind-key "C-c <right>" 'winner-undo)))

(t/use-package uniquify ; add dirs to buffer names when not unique
  :only-standalone t
  :ensure nil
  :init
  (progn
    (setq uniquify-buffer-name-style 'forward)))

(t/use-package subword
  :diminish subword-mode
  :only-standalone t
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
  :only-standalone t
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
    (bind-key "e" 't/eshell dired-mode-map)
    (bind-key "C-d" 'dired-kill-subdir dired-mode-map)
    (bind-key "C-c C-e" 'dired-toggle-read-only)
    (bind-key "C-x C-j" 'dired-jump)
    (bind-key "C-x M-j" '(lambda () (interactive) (dired-jump 1)))
    (bind-key "u" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
    (bind-key "M-<up>" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
    (bind-key "M-p" '(lambda () (interactive) (find-alternate-file "..")) dired-mode-map)
    (bind-key "M-<down>" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map)
    (bind-key "M-n" '(lambda () (interactive) (dired-find-alternate-file)) dired-mode-map)))

;; less verbose dired
(t/use-package dired-details
  :commands (dired-details-activate dired-details-toggle)
  :init
  (progn
    (setq dired-details-hidden-string "")
    (t/add-hook-defun 'dired-mode-hook t/hook-dired
                      (dired-hide-details-mode 1)
                      (all-the-icons-dired-mode))))

(use-package dired-subtree
  :commands dired-subtree-toggle
  :init
  (progn
    (setq dired-subtree-line-prefix "  ")
    (bind-key "TAB" 'dired-subtree-toggle dired-mode-map)))

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
  :only-standalone t
  :commands (neotree-toggle
             neotree-show
             neotree-hide
             neotree-find)
  :init
  (progn
    (setq neo-smart-open nil
          neo-window-fixed-size nil
          neo-create-file-auto-open t
          neo-mode-line-type 'none
          neo-show-updir-line nil
          neo-show-hidden-files t
          neo-auto-indent-point t)
    (t/add-hook 'neotree-mode-hook 'hl-line-mode)
    (when is-mac (setq neo-theme 'icons)))

  :config
  (progn

    (bind-key [f6] 'neotree-toggle)

    (defun neotree-change-root-up ()
      (interactive)
      (neotree-select-up-node))

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
                   ("u" . neotree-change-root-up)
                   ("M-<up>" . neotree-change-root-up)
                   ("I" . neotree-hidden-file-toggle)
                   ("q" . neotree-hide)
                   ("q" . neotree-hide)
                   ("TAB" . neotree-stretch-toggle)
                   ("RET" . neotree-enter)
                   ("M-<down>" . neotree-enter)
                   ("C" . neotree-change-root)))
      (eval `(evil-define-key (if *t-spacemacs* 'evilified 'normal) neotree-mode-map (kbd ,(car key)) ',(cdr key))))))

(t/use-package ace-window
  :commands ace-window
  :only-standalone t
  :config
  (progn
    (setq aw-keys '(?j ?k ?l ?a ?s ?d ?f ?g)
          aw-background t)))

(t/use-package ace-jump-mode
  :commands (ace-jump-mode
             ace-jump-char-mode
             ace-jump-line-mode
             ace-jump-word-mode)
  :init
  (progn
    (setq ace-jump-mode-gray-background t
          ace-jump-mode-case-fold t)))

(t/use-package evil-snipe
  :config
  (evil-snipe-override-mode 1))

(t/use-package undo-tree
  :only-standalone t
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
    (bind-key "C-x C-m" 'smex)
    (bind-key "C-c C-M" 'smex-major-mode-commands)
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
  :only-standalone t
  :defer 2
  :init
  (progn
    (setq company-idle-delay 0.2
          company-tooltip-align-annotations t
          company-tooltip-flip-when-above t
          company-show-numbers t ; nav with m-<n>
          company-selection-wrap-around t
          company-require-match nil
          company-backends (t/company-backends '()))
    (with-eval-after-load 'company
      (t/add-hook 'prog-mode-hook 'company-mode)))
  :config
  (progn
    (global-company-mode)
    (t/add-hook '+evil-esc-hook 'company-abort)
    (defun t/company-helm () (interactive) (company-abort) (completion-at-point))
    (t/bind-in 'company-active-map
               "TAB" 'company-complete-selection
               "C-w" 'evil-delete-backward-word
               "C-l" 'evil-delete-backward-word
               "C-u" 'backward-kill-sentence
               "C-n" 'company-select-next
               "C-p" 'company-select-previous
               "C-," #'t/company-helm)))

(t/use-package company-flx
  :after company
  :config
  (progn
    (company-flx-mode +1)))

(t/use-package company-web
  :commands (web-mode)
  :only-standalone t)

(t/use-package company-restclient
  :commands restclient-mode
  :config
  (t/add-company-backends-hook 'restclient-mode-hook 'company-restclient))

(t/use-package helm-xref
  :init
  (progn
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs))
  :config
  (progn
    (bind-key "M-." 'xref-find-definitions)
    (bind-key "M-?" 'xref-find-references)
    (bind-key "M-," 'xref-pop-marker-stack)))

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
    (bind-key "M-." 'xref-find-definitions)
    (bind-key "M-?" 'xref-find-references)
    (bind-key "M-," 'xref-pop-marker-stack)))

(t/use-package company-tern
  :commands tern-mode
  :diminish tern-mode
  :only-standalone t
  :config
  (progn
    (advice-add 'tern-find-definition :before 'xref-push-marker-stack) ; make pop-tag-mark work with tern
    (t/add-hook-defun 'tern-mode-hook t/hook-tern
                      (t/bind-in '(tern-mode-keymap evil-normal-state-local-map)
                                 "M-." 'tern-find-definition
                                 "M-," 'pop-tag-mark
                                 "C-M-." 'helm-etags-select))
    (setq tern-command (append tern-command '("--no-port-file")))))

(t/use-package etags-select
  :commands etags-select-find-tag-at-point
  :init
  (progn
    (setq helm-etags-fuzzy-match t
          etags-select-go-if-unambiguous t
          ;; fix helm-etags-select for huge regexps
          helm-fuzzy-matching-highlight-fn (lambda (file) file))
    (t/add-hook-defun 'etags-select-mode-hook t/hook-etags-select
                      (t/bind-in 'etags-select-mode-map
                                 "j" 'etags-select-next-tag
                                 "k" 'etags-select-previous-tag
                                 "RET" 'etags-select-goto-tag
                                 "M-RET" 'etags-select-goto-tag-other-window))))

(t/use-package etags-table
  :after etags-select
  :init
  (setq etags-table-search-up-depth 1 ; don't search upwards
        tags-file-name nil ; only use tags-table-list via etags-table
        etags-table-alist (list
                           `(,(t/user-file "Code/datainn/datainn/.*\\.jsx?$")
                             ,(t/user-file "Code/datainn/datainn/web/TAGS"))
                           `(".*\\.jsx?$"
                             ,(t/user-file ".nvm/versions/node/v8.2.1/src/node-8.x/TAGS") ; generated with `ctags -e -R lib`
                             ))))

(t/use-package helm-unicode
  :commands helm-unicode)

(t/use-package company-emoji :after company)

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
    (with-eval-after-load 'smartparens
      (setq sp-ignore-modes-list (delete 'minibuffer-inactive-mode sp-ignore-modes-list)))
    (sp-use-paredit-bindings)
    (bind-key "<backspace>" 'sp-backward-delete-char sp-keymap)
    (bind-key "<delete>" 'sp-delete-char sp-keymap)
    ;; interfers with e.g. org-mode, enable them specifically in lisp modes instead
    (unbind-key "M-<up>" sp-keymap)
    (unbind-key "M-<down>" sp-keymap)
    (unbind-key "M-?" sp-keymap)
    (unbind-key "C-<right>" sp-keymap)
    (unbind-key "C-<left>" sp-keymap)
    (bind-key "RET" #'t/newline-expand-braces)

    (dolist (mode-map (list
                       emacs-lisp-mode-map
                       lisp-mode-map
                       lisp-interaction-mode-map))
      (define-key mode-map ";" 'sp-comment))

    (with-eval-after-load 'clojure-mode
      (define-key clojure-mode-map ";" 'sp-comment))

    (t/add-hook '(web-mode-hook
                  js-mode-hook
                  java-mode-hook
                  text-mode-hook
                  restclient-mode-hook
                  ruby-mode
                  mark-down-mode)
                'turn-on-smartparens-mode)

    (with-eval-after-load 'web-mode
      (t/bind-in 'web-mode-map
                 "M-<up>" 'sp-splice-sexp-killing-backward
                 "M-<down>" 'sp-splice-sexp-killing-forward))
    (with-eval-after-load 'java-mode
      (t/bind-in 'java-mode-map
                 "M-<up>" 'sp-splice-sexp-killing-backward
                 "M-<down>" 'sp-splice-sexp-killing-forward))
    ;; enable in minibuffer
    (t/add-hook 'eval-expression-minibuffer-setup-hook #'(turn-on-smartparens-mode evil-cleverparens-mode))

    (defun t/enable-movement-for-lisp-mode (m)
      (lexical-let* ((mode (symbol-name m))
                     (mode-hook (intern (concat mode "-hook")))
                     (mode-map (intern (concat mode "-map"))))
        (add-hook mode-hook 'turn-on-smartparens-mode)
        (add-hook mode-hook 'evil-cleverparens-mode)

        ;; add M-<up/down> in lisp modes, not to steal them in org-mode
        (add-hook mode-hook
                  (lambda nil
                    (eval
                     `(progn
                        (bind-key "M-<up>" 'sp-splice-sexp-killing-backward ,mode-map)
                        (bind-key "M-<down>" 'sp-splice-sexp-killing-forward ,mode-map)))))
        (eval
         `(t/bind-in (quote ,mode-map)
                     "M-<left>" #'t/backward-down-sexp
                     "M-<right>" #'t/forward-down-sexp
                     "M-S-<left>" #'t/backward-sexp
                     "M-S-<right>" #'t/forward-sexp
                     "C-<right>" #'sp-forward-slurp-sexp
                     "C-<left>" #'sp-forward-barf-sexp))))

    (dolist (mode (list
                   'emacs-lisp-mode
                   'lisp-mode
                   'lisp-interaction-mode))
      (t/enable-movement-for-lisp-mode mode))

    (with-eval-after-load 'clojure-mode (t/enable-movement-for-lisp-mode 'clojure-mode))
    (with-eval-after-load 'ielm-mode (t/enable-movement-for-lisp-mode 'ielm-mode))
    (with-eval-after-load 'scheme-mode (t/enable-movement-for-lisp-mode 'scheme-mode))
    (t/add-hook-defun 'minibuffer-inactive-mode-hook t/hook-minibuffer
                      (t/bind-in 'minibuffer-local-map
                                 "M-<up>" 'sp-splice-sexp-killing-backward
                                 "M-<down>" 'sp-splice-sexp-killing-forward
                                 "M-<left>" #'t/backward-down-sexp
                                 "M-<right>" #'t/forward-down-sexp
                                 "M-S-<left>" #'t/backward-sexp
                                 "M-S-<right>" #'t/forward-sexp
                                 "C-<right>" #'sp-forward-slurp-sexp
                                 "C-<left>" #'sp-forward-barf-sexp))

    (defun t/disable-quote-pairs-for-mode (mode)
      (sp-local-pair mode "`" nil :actions nil)
      (sp-local-pair mode "'" nil :actions nil))

    (dolist (mode '(emacs-lisp-mode
                    clojure-mode
                    ielm-mode
                    lisp-mode
                    lisp-interaction-mode
                    minibuffer-inactive-mode
                    scheme-mode))
      (t/disable-quote-pairs-for-mode mode))

    (sp-with-modes 'emacs-lisp-mode
      (sp-local-pair "`" "'" :when '(sp-in-docstring-p)))

    (t/def-pairs ((paren . "(")
                  (bracket . "[")
                  (brace . "{")
                  (single-quote . "'")
                  (double-quote . "\"")
                  (back-quote . "`")))

    (bind-key "s-(" 't/wrap-with-parens)
    (bind-key "s-)" 't/paredit-wrap-round-from-behind)
    (bind-key "M-s-(" 't/wrap-with-braces)
    (bind-key "M-s-[" 't/wrap-with-brackets))

  :config
  (progn
    (t/bind-in 'text-mode-map
               "C-<right>" #'sp-forward-slurp-sexp
               "C-<left>" #'sp-forward-barf-sexp)
    (with-eval-after-load 'web-mode
      (t/bind-in 'web-mode-map
                 "C-<right>" #'sp-forward-slurp-sexp
                 "C-<left>" #'sp-forward-barf-sexp))))

(t/use-package writeroom-mode
  :commands writeroom-mode)

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
             helm-mini
             helm-projectile
             helm-projectile-ag)
  :only-standalone t
  :diminish helm-mode
  :config
  (progn
    (require 'helm-config)
    (with-eval-after-load 'helm-command
      (t/bind-in '(helm-map helm-M-x-map)
                 "C-w" 'backward-kill-word
                 "C-u" 'backward-kill-sentence
                 "C-c u" 'universal-argument))
    (with-eval-after-load 'helm-files
      (t/bind-in 'helm-find-files-map
                 "s-<down>" 'helm-execute-persistent-action
                 "s-<up>" 'helm-find-files-up-one-level
                 "C-k" 'helm-find-files-up-one-level))
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
                  helm-follow-mode-persistent t ; keep follow mode on, after on once
                  helm-ff-skip-boring-files t
                  helm-quick-update t
                  helm-M-x-requires-pattern nil)
    (helm-mode 1)
    (t/add-hook-defun 'helm-after-initialize-hook t/hide-cursor-in-helm-buffer
                      (with-helm-buffer
                        (setq cursor-in-non-selected-windows nil)))
    (set-face-attribute 'helm-source-header nil :height 1)
    (with-eval-after-load 'neotree
      (t/add-hook 'helm-before-initialize-hook 'neotree-hide))))

(t/use-package helm-ag
  :only-standalone t
  :commands helm-ag
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
  :only-standalone t
  :commands helm-projectile)

(t/use-package helm-descbinds
  :only-standalone t
  :commands helm-descbinds
  :config
  (progn
    (helm-descbinds-mode)
    (setq helm-descbinds-window-style 'split)))

(t/use-package helm-dash
  :commands helm-dash)

(t/use-package helm-google
  :commands helm-google)

(t/use-package helm-swoop
  :only-standalone t
  :commands helm-swoop
  :config
  (t/bind-in 'helm-swoop-edit-map
             "C-c C-c" 'helm-swoop--edit-complete
             "C-c C-k" 'helm-swoop--edit-cancel))

(t/use-package visual-regexp
  :commands vr/query-replace
  :config
  (progn
    (bind-key "M-%" 'vr/query-replace)))

(t/use-package dash-at-point
  :commands dash-at-point
  :config
  (progn
    (bind-key "C-c C-j" 'dash-at-point)))

(t/use-package expand-region
  :commands (er/expand-region er/contract-region)
  :only-standalone t
  :init
  (progn
    (bind-key (if is-mac "M-@" "M-'") 'er/expand-region)
    (bind-key (if is-mac "M-*" "M-§") 'er/contract-region)))

(t/use-package transpose-frame
  :commands transpose-frame)

(t/use-package yasnippet
  :diminish yas-minor-mode
  :only-standalone t
  :init
  (progn
    (setq yas-snippet-dirs '(t-dir-snippets)
          ;; remove dropdowns
          yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
          ;; silence
          yas-verbosity 1
          ;; wrap around region
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
          ag-highlight-search t
          ag-project-root-function (lambda (d) (projectile-project-root)))))

(t/use-package wgrep :after ag)
(t/use-package wgrep-ag :after ag)

(t/use-package autorevert
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
  :only-standalone t
  :init
  (progn
    (setq smooth-scroll-margin 4
          mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; two lines at a time
          mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
          mouse-wheel-follow-mouse 't))
  :config
  (progn
    (smooth-scrolling-mode)
    (enable-smooth-scroll-for-function previous-line)
    (enable-smooth-scroll-for-function next-line)
    (enable-smooth-scroll-for-function isearch-repeat)))

(t/use-package highlight-parentheses
  :only-standalone t
  :diminish highlight-parentheses-mode
  :defer 1
  :init
  (progn
    (setq hl-paren-colors (-repeat 26 "DeepPink"))
    (t/add-hook 'prog-mode-hook 'highlight-parentheses-mode))
  :config
  (progn
    (set-face-foreground 'show-paren-match "Green")))

(t/use-package highlight-escape-sequences
  :commands hes-mode
  :init
  (progn
    (t/add-hook 'prog-mode-hook 'hes-mode))
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
    (setq highlight-symbol-idle-delay 0.2)
    (t/add-hook 'prog-mode-hook 'highlight-symbol-mode)))

(t/use-package highlight-numbers
  :defer 1
  :only-standalone t
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
(with-eval-after-load 'shr
  ;; don't truncate lines in eww-mode
  ;;(setq shr-width nil)
  (defun shr-fill-text (text) text)
  (defun shr-fill-lines (start end) nil)
  (defun shr-fill-line () nil)

  ;; wrap lines
  (t/add-hook-defun 'eww-after-render-hook t/hook-eww-trunc
                    (toggle-truncate-lines -1))

  ;; not to large images
  (setq shr-use-fonts nil
        shr-max-image-proportion 0.6
        shr-ignore-cache t
        ))

(t/use-package hackernews
  :commands hackernews
  :config
  (progn
    (advice-add 'hackernews :after 'evil-emacs-state)
    (with-eval-after-load 'eww
      (advice-add 'eww-mode :after 'evil-emacs-state)
      (t/bind-in 'eww-mode-map
                 "b" 'eww-browse-with-external-browser
                 "M-n" 'forward-paragraph
                 "M-b" 'backward-paragraph))
    (t/add-hook-defun 'eww-mode-hook t/hook-eww
                      (writeroom-mode)
                      (visual-line-mode))
    (t/bind-in 'hackernews-map
               "<return>" 'hackernews-button-browse-internal
               "b" (lambda nil
                     (interactive)
                     (hackernews-browse-url-action
                      (button-at (point)))))))

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
  :commands calendar
  :init
  (progn
    (setq calendar-week-start-day 1
          calendar-date-style 'iso)))

(t/use-package suggest
  :commands suggest)

;; save more recent files
(t/use-package recentf
  :defer 1
  :only-standalone t
  :init
  (progn
    (setq recentf-max-saved-items 1000
          recentf-auto-cleanup 'never))
  :config
  (progn
    (t/idle-timer recentf-auto-save-timer #'recentf-save-list 1)
    (recentf-mode 1)))

(t/use-package linum-relative
  :commands linum-relative-mode
  :init
  (progn
    (t/add-hook-setq 'linum-mode-hook linum-format " %d ")
    (t/add-hook-setq 'linum-relative-mode-hook linum-relative-format " %3s ")
    (linum-relative-mode 0)))

(t/use-package projectile
  :diminish projectile-mode
  :only-standalone t
  :commands (projectile-mode
             helm-projectile
             projectile-project-root
             projectile-relevant-known-projects)
  :init
  (progn
    (setq shell-file-name "/bin/sh" ; cause zsh makes projectile unable to find the git repo
          projectile-completion-system 'helm
          projectile-require-project-root nil
          projectile-known-projects-file (locate-user-emacs-file ".cache/projectile.projects")
          projectile-cache-file (locate-user-emacs-file ".cache/projectile.cache")
          projectile-enable-caching t
          projectile-project-root-files '(".git" ".hg" ".svn" ".project" "package.json" "setup.py" "Gemfile" "build.gradle")))
  :config
  (progn
    (t/add-to-list 'projectile-globally-ignored-directories '("elpa-backups" "node_modules" "target" "dist" ".idea"))
    (t/add-to-list 'projectile-globally-ignored-files '("**.bundle.js" "**.build.js" ".DS_Store" "projectile.cache" "custom.el"))
    (t/add-to-list 'grep-find-ignored-files '("**.bundle.js" "**.build.js" ".DS_Store" "custom.el"))
    (projectile-global-mode +1)))

(t/use-package dumb-jump
  :commands dumb-jump-go
  :init
  (progn
    (setq dumb-jump-selector 'helm))
  (progn
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
    (t/add-hook-defun 'java-mode-hook t/hook-java (aggressive-indent-mode 0))
    (t/add-hook 'prog-mode-hook 'aggressive-indent-mode)
    (t/declare-prefix "t" "Toggles"
                      "a" 'aggressive-indent-mode)))

(t/use-package beginend
  :commands (beginend-prog-mode-goto-beginning
             beginend-prog-mode-goto-end)
  :init
  (progn
    (bind-key "M-<" 'beginend-prog-mode-goto-beginning)
    (bind-key "M->" 'beginend-prog-mode-goto-end)))

(use-package whitespace
  :ensure nil
  :init
  (progn
    (t/add-hook 'before-save-hook 'whitespace-cleanup)
    (t/add-hook-defun '(prog-mode-hook text-mode-hook git-commit-mode-hook) t/hook-whitespace
                      (setq-local whitespace-style '(face tabs tab-mark trailing))
                      (whitespace-mode 1))))

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

    (with-eval-after-load 'evil-leader
      (t/declare-prefix "aa" "drawing"
                        "t" #'t/artist-mode
                        "p" 'artist-select-op-pen-line
                        "r" 'artist-select-op-rectangle
                        "c" 'artist-select-op-circle
                        "e" 'artist-select-op-ellipse
                        "s" 'artist-select-op-square))))

(t/use-package try
  :commands try)

(t/use-package selectric-mode ; lol
  :commands selectric-mode)

(defun t-editor/config ()

  (with-eval-after-load 're-builder (setq reb-re-syntax 'rx))
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
                    "b" 'browse-url-at-point
                    "C" 'calc-dispatch
                    "d" 'dired-jump
                    "h" 'hackernews
                    "i" 't/open-in-intellij
                    "p" 'list-processes
                    "m" 'helm-spotify
                    "R" #'t/toggle-regex-mode
                    "se" 't/eshell
                    "st" 'ansi-term
                    "sT" 'term
                    "S" 'suggest)

  (t/declare-prefix "fe" "Editor")

  (t/declare-prefix "fe" "Files"
                    "R" 'config-reload
                    "i" 't/helm-files-emacs-init-files)

  (t/declare-prefix "fep" "Packages"
                    "i" 'package-install
                    "r" 'package-refresh-contents
                    "l" 'paradox-list-packages
                    "R" 'package-reinstall
                    "U" 't/upgrade-packages)

  (t/declare-prefix "t" "Toggle"
                    "d" 'toggle-debug-on-error
                    "f" 'toggle-frame-fullscreen
                    "t" 't/load-theme-cycle
                    "n" #'t/toggle-line-numbers
                    "r" #'t/toggle-relative-line-numbers
                    "l" 'hl-line-mode
                    "L" 'visual-line-mode
                    "w" 'whitespace-mode
                    "W" 'writeroom-mode
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
                    "n" 'next-buffer
                    "R" 'revert-buffer)

  (t/declare-prefix "d" "Doc"
                    "d" 'dash-at-point
                    "s" 'dash-at-point-with-docset)

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
                    "f" 'helm-find-files
                    "l" 't/neotree-open-file
                    "j" 'dired-jump
                    "J" 'dired-jump-other-window
                    "d" 'delete-frame
                    "g" 'ffap
                    "o" 't/open-in-desktop
                    "r" 'helm-recentf)

  (t/declare-prefix "h" "Help"
                    "h" #'t/describe
                    "a" 'helm-apropos
                    "l" 'helm-locate-library
                    "i" 'helm-info-at-point
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
                    "j" 'dumb-jump-go
                    "c" 'ace-jump-char-mode
                    "l" 'ace-jump-line-mode
                    "W" 'ace-jump-word-mode)

  (t/declare-prefix "c" "Comment/Complete"
                    "l" 't/helm-find-and-insert-line-from-project)

  (t/declare-prefix "h" "Highlight"
                    "H" 'highlight-symbol
                    "n" 'highlight-symbol-next
                    "N" 'highlight-symbol-prev)

  (t/declare-prefix "p" "Project"
                    "b" 'helm-browse-project
                    "c" 'projectile-switch-project
                    "d" 't/projectile-dired
                    "f" 'helm-projectile
                    "F" 'projectile-find-file-dwim
                    "g" 't/projectile-magit-status
                    "G" 'projectile-regenerate-tags
                    "k" 'projectile-kill-buffers
                    "l" 't/neotree-open-file
                    "o" 't/open-in-desktop
                    "p" 'projectile-find-file-in-known-projects
                    "s" 't/projectile-helm-ag
                    "R" 'projectile-replace
                    "S" 'projectile-save-project-buffers
                    "t" 'projectile-find-test-file)

  (t/declare-prefix "s" "Search"
                    "s" 'helm-swoop
                    "b" (t/macro-helm-ag-insert 'word helm-ag-buffers)
                    "f" (t/macro-helm-ag-insert 'word helm-ag-this-file)
                    "p" 'helm-projectile-ag
                    "a" 'helm-multi-swoop-all
                    "m" 'helm-multi-swoop
                    "t" 'etags-select-find-tag-at-point
                    "T" 'helm-etags-select
                    "i" 'helm-imenu
                    "I" 'helm-imenu-in-all-buffers)

  (t/declare-prefix "sw" "Search Internet"
                    "i" 'helm-google
                    "g" 'helm-google
                    "G" 'helm-google-suggest
                    "w" 'helm-wikipedia-suggest))

(provide 't-editor)
