;;; -*- lexical-binding: t; -*-
(t/use-package evil
  :only-standalone t
  :init
  (setq evil-want-C-d-scroll t
        evil-want-C-u-scroll t))

(t/use-package evil-anzu
  :only-standalone t
  :init
  (progn
    (setq anzu-cons-mode-line-p nil
          anzu-minimum-input-length 1
          anzu-search-threshold 100)
    (t/add-hook '+evil-esc-hook 'anzu--reset-status)))

(t/use-package evil-escape
  :only-standalone t
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
          evil-leader/non-normal-prefix "C-"))
  :config
  (progn
    (evil-leader/set-leader t-leader)
    (evil-mode nil)
    (global-evil-leader-mode)
    (evil-mode 1)

    (defun spacemacs/evil-yank-to-end-of-line ()
      "Yank from point to end of line."
      (interactive)
      (evil-yank (point) (point-at-eol)))

    (bind-key "Y" 'spacemacs/evil-yank-to-end-of-line evil-normal-state-map)
    (bind-key "Y" 'spacemacs/evil-yank-to-end-of-line evil-motion-state-map)))

(t/use-package evil-matchit
  :only-standalone t
  :commands evilmi-jump-items
  :config
  (progn
    (global-evil-matchit-mode 1)))

(t/use-package evil-visualstar
  :only-standalone t
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :config
  (progn
    (bind-key "*" 'evil-visualstar/begin-search-forward evil-visual-state-map)
    (bind-key "#" 'evil-visualstar/begin-search-backward evil-visual-state-map)))

(t/use-package evil-cleverparens
  :diminish evil-cleverparens-mode
  :defer 1
  :init
  (progn (setq evil-cleverparens-use-additional-bindings nil
               evil-cleverparens-use-regular-insert t))
  :config
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))
    (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))))

(t/use-package evil-surround
  :only-standalone t
  :defer 1
  :config
  (progn
    (global-evil-surround-mode 1)
    ;; the opposite of vim, like spacemacs
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))

(t/use-package evil-multiedit
  :commands evil-multiedit-match-symbol-and-next
  :init
  (progn
    (setq evil-multiedit-follow-matches t)
    (bind-key "M-d" 'evil-multiedit-match-symbol-and-next evil-normal-state-map)
    (bind-key "C-M-d" 'evil-multiedit-restore evil-normal-state-map)
    ;; seems to make helm-projectile disappear?
    ;;(advice-add 'evil-multiedit-abort :after #'+evil*attach-escape-hook)
    )
  :config
  (progn
    (evil-multiedit-default-keybinds)
    (unbind-key "M-d" evil-insert-state-map)
    (bind-key "gn" 'evil-multiedit--visual-line evil-multiedit-state-map)

    (progn
      (setq evil-multiedit-store-in-search-history t)

      (defun t/mc-prev ()
        (interactive)
        (evil-ex-search-previous)
        (evil-ex-nohighlight)
        (when (not (iedit-find-current-occurrence-overlay))
          (evil-multiedit-toggle-or-restrict-region)))

      (defun t/mc-next ()
        (interactive)
        (evil-ex-search-next)
        (evil-ex-nohighlight)
        (when (not (iedit-find-current-occurrence-overlay))
          (evil-multiedit-toggle-or-restrict-region)))

      (defun t/mc-skip-prev ()
        (interactive)
        (evil-multiedit-toggle-or-restrict-region)
        (evil-ex-search-previous)
        (evil-ex-nohighlight)
        (when (not (iedit-find-current-occurrence-overlay))
          (evil-multiedit-toggle-or-restrict-region)))

      (defun t/mc-skip-next ()
        (interactive)
        (evil-multiedit-toggle-or-restrict-region)
        (evil-ex-search-next)
        (evil-ex-nohighlight)
        (when (not (iedit-find-current-occurrence-overlay))
          (evil-multiedit-toggle-or-restrict-region)))

      (t/bind-in 'evil-multiedit-state-map
                 "M-j" #'t/mc-next
                 "M-k" #'t/mc-prev
                 "M-J" #'t/mc-skip-next
                 "M-K" #'t/mc-skip-prev))))

(t/use-package evil-commentary
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :init (evil-commentary-mode 1))

(defun t-evil/vars ()
  (progn
    (defvar t-evil-major-modes '(compilation-mode
                                 special-mode
                                 calendar-mode
                                 git-rebase-mode
                                 diff-mode)
      "Major modes that should trigger evil emacs state when changed to.")
    (with-eval-after-load 'evil
      (t/add-hook-defun 'after-change-major-mode-hook t/hook-major-mode
                        (when (member major-mode t-evil-major-modes)
                          (evil-emacs-state))))

  (setq evil-default-state 'normal
        evil-insert-skip-empty-lines t
        evil-search-module 'evil-search)
  (t/add-hook '+evil-esc-hook 'evil-ex-nohighlight)))

(defun t-evil/funcs ()
  (t/add-hook '(git-commit-mode-hook org-capture-mode-hook) 'evil-insert-state)

  (defun t/init-evil-cursors (&rest _)
    "Change cursors after theme colors have loaded."
    (setq evil-default-cursor (face-background 'cursor nil t)
          evil-emacs-state-cursor  `(,(face-foreground 'warning) box)
          evil-normal-state-cursor 'box
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow))
  (advice-add #'load-theme :after #'t/init-evil-cursors)

  (defun t/toggle-evil-local-mode ()
    "Toggles evil-local-mode and updates the cursor. The `evil-*-state-cursor's seem to work by default, but not when toggling evil-local-mode (in emacsclients) off and on, so help it."
    (interactive)
    (if (equal evil-state 'emacs)
        (evil-normal-state)
      (evil-emacs-state))))

(defun t-evil/config ()
  (if is-mac
      (bind-key "C-'" 't/toggle-evil-local-mode)
    (bind-key "C-|" 't/toggle-evil-local-mode))

  (progn
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
            (t (run-hook-with-args-until-success '+evil-esc-hook)))))

  ;; motions keys for help buffers
  (evil-define-key 'motion help-mode-map (kbd "q") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
  (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

  ;; i_Ctrl-o - C-o from hybrid mode, like in vim insert mode
  (evil-define-key 'hybrid global-map (kbd "C-o") 'evil-execute-in-normal-state)

  (defun t/paste-prev ()
    (interactive)
    (evil-paste-pop -1))

  ;; some emacs stuff is useful, in terminals etc
  ;; http://stackoverflow.com/a/16226006
  (t/bind-in '(evil-normal-state-map
               evil-insert-state-map
               evil-visual-state-map
               evil-motion-state-map)
             "C-a" 't/smart-beginning-of-line
             "C-b" 'evil-backward-char
             "C-d" 'evil-delete-char
             "C-e" 'end-of-line
             "C-f" 'evil-forward-char
             "C-k" 'kill-line
             "C-n" 'evil-next-line
             "C-p" 'evil-previous-line
             "C-w" 'evil-delete-backward-word
             "M-y" 'helm-show-kill-ring
             "C-y" 'evil-paste-pop)

  (t/bind-in '(evil-normal-state-map
               evil-visual-state-map)
             "Q" 'call-last-kbd-macro
             "C-y" 'evil-paste-pop ; cycle after pasting with p
             "C-S-y" 't/evil-paste-prev)

  ;; esc ought to quit
  (bind-key [escape] 'keyboard-quit evil-normal-state-map)
  (bind-key [escape] 'keyboard-quit evil-visual-state-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

  (defun t/keyboard-quit-advice (fn &rest args)
    (let ((region-was-active (region-active-p)))
      (unwind-protect (apply fn args)
        (+evil*attach-escape-hook))))
  (advice-add 'keyboard-quit :around #'t/keyboard-quit-advice)

  ;; macro camelCase to snakeCase
  (evil-set-register ?c [?: ?s ?/ ?\\ ?\( ?\[ ?a ?- ?z ?0 ?- ?9 ?\] ?\\ ?\) ?\\ ?\( ?\[ ?A ?- ?Z ?0 ?- ?9 ?\] ?\\ ?\) ?/ ?\\ ?1 ?_ ?\\ ?l ?\\ ?2 ?/ ?g]))

(provide 't-evil)
