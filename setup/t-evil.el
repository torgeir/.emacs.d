;;; -*- lexical-binding: t; -*-
(setq evil-want-C-d-scroll t
      evil-want-C-u-scroll t
      evil-want-keybinding nil
      evil-want-integration t
      evil-want-Y-yank-to-eol nil
      evil-move-beyond-eol nil)

(setq evil-default-state 'normal
      evil-insert-skip-empty-lines t
      evil-search-module 'evil-search)

(t/use-package evil
  :init
  (progn
    ;; https://emacs.stackexchange.com/a/15054
    (fset 'evil-visual-update-x-selection 'ignore)))

(t/use-package evil-anzu
  :init
  (progn
    (setq anzu-cons-mode-line-p nil
          anzu-minimum-input-length 1
          anzu-search-threshold 100)))

(t/use-package evil-escape
  :after evil
  :init
  (progn
    (setq-default evil-escape-key-sequence "jk"
                  evil-escape-delay 0.1))
  :config
  (evil-escape-mode))

(t/use-package evil-leader
  :after evil
  :init
  (progn
    (setq evil-leader/in-all-states t
          evil-leader/non-normal-prefix t-emacs-leader))
  :config
  (progn
    (evil-leader/set-leader t-leader)
    (evil-mode nil)
    (global-evil-leader-mode)
    (evil-mode 1)
    (t/bind-in '(evil-normal-state-map evil-motion-state-map)
      "Y" 't/evil-yank-to-end-of-line)))

(t/use-package evil-collection
  :after evil
  :init
  (progn
    (setq evil-collection-key-blacklist '("ZZ" "ZQ"))
    (evil-collection-init)
    (t/after org
      (evil-collection-define-key 'normal 'outline-mode-map (kbd "<tab>") 'org-cycle))
    (progn
      ;; https://github.com/jtbm37/all-the-icons-dired/pull/19
      (t/after evil-collection-wdired
        (defvar-local +wdired-icons-enabled nil)
        (defun +wdired-before-start-advice ()
          "Execute when switching from `dired' to `wdired'."
          (setq +wdired-icons-enabled (if (bound-and-true-p all-the-icons-dired-mode)
                                          1 0))
          (when (bound-and-true-p all-the-icons-dired-mode)
            (all-the-icons-dired-mode 0)))
        (defun +wdired-after-finish-advice ()
          "Execute when switching from `wdired' to `dired'"
          (when (boundp 'all-the-icons-dired-mode)
            (all-the-icons-dired-mode +wdired-icons-enabled)))
        (advice-add 'wdired-change-to-wdired-mode :before #'+wdired-before-start-advice)
        (advice-add 'wdired-change-to-dired-mode :after #'+wdired-after-finish-advice)))))

(t/use-package evil-matchit
  :commands evilmi-jump-items
  :config
  (progn
    (global-evil-matchit-mode 1)))

(t/use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (progn
    (t/bind-in 'evil-visual-state-map
      "*" 'evil-visualstar/begin-search-forward
      "#" 'evil-visualstar/begin-search-backward)))

(t/use-package evil-cleverparens
  :diminish evil-cleverparens-mode
  :defer 1
  :init
  (progn
    (t/add-hook-defun
     'evil-cleverparens-enabled-hook t-evil-cp-mode-hook
     (evil-define-key 'visual evil-cleverparens-mode-map (kbd "M-d") 'evil-multiedit-match-symbol-and-next)
     (evil-define-key 'normal evil-cleverparens-mode-map (kbd "M-d") 'evil-multiedit-match-symbol-and-next))
    (setq evil-cleverparens-use-additional-bindings t
          evil-cleverparens-use-regular-insert t))
  :config
  (t/after evil-surround
    (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))
    (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))))

(t/use-package evil-surround
  :defer 1
  :config
  (progn
    (global-evil-surround-mode 1)
    ;; the opposite of vim, like spacemacs
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))

(t/use-package evil-snipe
  :defer 1
  :init
  (t/add-hook-defun 'prog-mode-hook t-hook-snipe
                    (evil-snipe-local-mode 1)
                    (evil-snipe-override-local-mode 1)))

(t/use-package evil-multiedit
  :commands evil-multiedit-match-symbol-and-next
  :init
  (progn
    (setq evil-multiedit-follow-matches t)
    (t/bind-in 'evil-normal-state-map
      "M-d" 'evil-multiedit-match-symbol-and-next
      "C-M-r" 'evil-multiedit-restore))
  :config
  (progn
    (evil-multiedit-default-keybinds)
    (unbind-key "M-d" evil-insert-state-map)
    (unbind-key "C-M-D" evil-normal-state-map)
    (bind-key "gn" 'evil-multiedit--visual-line evil-multiedit-state-map)

    (progn
      (setq evil-multiedit-store-in-search-history t)

      (defun t/mc-skip-prev ()
        (interactive)
        (evil-multiedit-toggle-or-restrict-region)
        (evil-multiedit-match-and-prev))

      (defun t/mc-skip-next ()
        (interactive)
        (evil-multiedit-toggle-or-restrict-region)
        (evil-multiedit-match-and-next))

      (t/bind-in 'evil-multiedit-state-map
        "M-j" #'t/mc-skip-next
        "M-k" #'t/mc-skip-prev))))

(t/use-package evil-commentary
  :defer 1
  :init (evil-commentary-mode))

(t/use-package google-translate
  :commands google-translate-at-point)

(t/use-package evil-goggles
  :defer 1
  :init
  (progn
    (setq evil-goggles-duration 0.2
          evil-goggles-async-duration 0.2
          evil-goggles-pulse t))
  :config
  (t/add-hook-defun 'prog-mode-hook t/hook-goggles
                    (evil-goggles-mode)
                    (evil-goggles-use-magit-faces)))

;; dependencies of evil-extra-operator
(use-package highlight)
(use-package fold-this
  :after highlight)

(t/use-package evil-extra-operator
  :after fold-this
  :defer 1
  :init
  (setq evil-extra-operator-org-capture-key "gC")
  :config
  (global-evil-extra-operator-mode 1))

(defvar t-evil-major-modes '(compilation-mode
                             special-mode
                             calendar-mode
                             git-rebase-mode
                             diff-mode)
  "Major modes that should trigger evil emacs state when changed to.")
(t/after evil
  (t/add-hook-defun 'after-change-major-mode-hook t/hook-major-mode
                    (when (member major-mode t-evil-major-modes)
                      (evil-emacs-state))))

(defun t-evil/config ()
  (t/add-hook '(git-commit-mode-hook org-capture-mode-hook) 'evil-insert-state)

  (defun t/init-evil-cursors (&rest _)
    "Change cursors after theme colors have loaded."
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-emacs-state-cursor  `(,(face-foreground 'warning) box)
          evil-normal-state-cursor 'box
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'t/init-evil-cursors)

  (defvar +evil-esc-hook '(t)
    "A hook run after ESC is pressed in normal mode (invoked by
`evil-force-normal-state'). If a hook returns non-nil, all hooks after it are
ignored.")

  (defun +evil*attach-escape-hook (&optional ignore)
    "Run all `+evil-esc-hook' hooks. If any returns non-nil, stop there."
    (cond (;; quit the minibuffer if open.
           (minibuffer-window-active-p (minibuffer-window))
           (abort-recursive-edit))
          ;; disable ex search buffer highlights.
          ((evil-ex-hl-active-p 'evil-ex-search)
           (evil-ex-nohighlight))
          ;; escape anzu number of matches
          ((and (featurep 'anzu)
                anzu--state)
           (anzu--reset-status))
          ;; remove highlights
          ((and (featurep 'highlight-symbol)
                highlight-symbol-mode)
           (highlight-symbol-remove-all))
          ;; Run all escape hooks. If any returns non-nil, then stop there.
          (t (run-hook-with-args-until-success '+evil-esc-hook))))
  (advice-add #'evil-force-normal-state :after #'+evil*attach-escape-hook)

  ;; motions keys for help buffers
  (evil-define-key 'motion help-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
  (evil-define-key 'motion help-mode-map (kbd "L") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "H") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

  ;; motion keys for info mode
  (evil-define-key 'normal Info-mode-map (kbd "H") 'Info-history-back)
  (evil-define-key 'normal Info-mode-map (kbd "L") 'Info-history-forward)
  (unbind-key (kbd "h") Info-mode-map)
  (unbind-key (kbd "l") Info-mode-map)

  ;; i_Ctrl-o - C-o from hybrid mode, like in vim insert mode
  (evil-define-key 'hybrid global-map (kbd "C-o") 'evil-execute-in-normal-state)

  ;; some emacs stuff is useful, in terminals etc
  ;; http://stackoverflow.com/a/16226006
  (t/bind-in '(evil-normal-state-map
               evil-insert-state-map
               evil-visual-state-map
               evil-motion-state-map)
    "C-a" 't/smart-beginning-of-line
    "C-e" 'end-of-line
    "C-b" 'evil-backward-char
    "C-f" 'evil-forward-char
    "C-k" 'kill-line
    "C-n" 'evil-next-line
    "C-p" 'evil-previous-line
    "C-w" 'evil-delete-backward-word
    "M-y" 'helm-show-kill-ring)

  (t/bind-in 'evil-insert-state-map
    "C-d" 'evil-delete-char
    "C-u" (t/lambda (kill-line 0)))

  (t/bind-in '(evil-normal-state-map
               evil-visual-state-map)
    "Q" 'call-last-kbd-macro
    "C-y" 'evil-paste-pop ; cycle after pasting with p
    "C-S-y" (t/lambda (evil-paste-pop-next 1)))

  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

  ;; macro camelCase to snakeCase
  (evil-set-register ?c [?: ?s ?/ ?\\ ?\( ?\[ ?a ?- ?z ?0 ?- ?9 ?\] ?\\ ?\) ?\\ ?\( ?\[ ?A ?- ?Z ?0 ?- ?9 ?\] ?\\ ?\) ?/ ?\\ ?1 ?_ ?\\ ?l ?\\ ?2 ?/ ?g]))

(provide 't-evil)
