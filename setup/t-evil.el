(t/use-package evil
  :only-standalone t)

(t/use-package evil-anzu
  :only-standalone t
  :init
  (progn
    (setq anzu-cons-mode-line-p nil
          anzu-minimum-input-length 1
          anzu-search-threshold 100)
    (add-hook '+evil-esc-hook 'anzu--reset-status)))

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
          evil-leader/non-normal-prefix "C-c C-"))
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
  :after evil
  :diminish evil-cleverparens-mode
  :init
  (progn (setq evil-cleverparens-use-additional-bindings nil
               evil-cleverparens-use-regular-insert t))
  :config
  (with-eval-after-load 'evil-surround
    (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete))
    (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))))

(t/use-package evil-surround
  :only-standalone t
  :after evil
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
    (advice-add 'evil-multiedit-abort :after #'+evil*attach-escape-hook))
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

      (bind-key "M-j" #'t/mc-next evil-multiedit-state-map)
      (bind-key "M-k" #'t/mc-prev evil-multiedit-state-map)
      (bind-key "M-J" #'t/mc-skip-next evil-multiedit-state-map)
      (bind-key "M-K" #'t/mc-skip-prev evil-multiedit-state-map))

    ;; TODO remove when https://github.com/syl20bnr/evil-escape/pull/77 is merged
    (defun evil-escape-func ()
      "Return the function to escape from everything."
      (pcase evil-state
        (`normal (evil-escape--escape-normal-state))
        (`motion (evil-escape--escape-motion-state))
        (`insert 'evil-normal-state)
        (`emacs (evil-escape--escape-emacs-state))
        (`hybrid (evil-escape--escape-emacs-state))
        (`evilified (evil-escape--escape-emacs-state))
        (`visual 'evil-exit-visual-state)
        (`replace 'evil-normal-state)
        (`lisp 'evil-lisp-state/quit)
        (`iedit 'evil-iedit-state/quit-iedit-mode)
        (`iedit-insert 'evil-iedit-state/quit-iedit-mode)
        (`multiedit 'evil-multiedit-abort)
        (`multiedit-insert 'evil-multiedit-abort)
        (_ (evil-escape--escape-normal-state))))))

(comment
 (t/use-package org-evil
   :after evil))

(t/use-package evil-commentary
  :after evil
  :commands (evil-commentary evil-commentary-yank evil-commentary-line)
  :config (evil-commentary-mode 1))

(defun t-evil/vars ()
  (progn
    (defvar t-evil-major-modes '(compilation-mode
                                 special-mode
                                 calendar-mode
                                 git-rebase-mode
                                 diff-mode)
      "Major modes that should trigger evil emacs state when changed to.")
    (add-hook 'after-change-major-mode-hook
              (lambda ()
                (when (member major-mode t-evil-major-modes)
                  (evil-emacs-state)))))

  (setq evil-default-state 'normal
        evil-insert-skip-empty-lines t
        evil-search-module 'evil-search)
  (add-hook '+evil-esc-hook 'evil-ex-nohighlight))

(defun t-evil/funcs ()
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

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

  ;; some emacs stuff is useful, in terminals etc
  ;; http://stackoverflow.com/a/16226006
  (bind-key "C-e" 'evil-end-of-line evil-normal-state-map)
  (bind-key "C-e" 'end-of-line evil-insert-state-map)
  (bind-key "C-e" 'evil-end-of-line evil-visual-state-map)
  (bind-key "C-e" 'evil-end-of-line evil-motion-state-map)
  (bind-key "C-f" 'evil-forward-char evil-normal-state-map)
  (bind-key "C-f" 'evil-forward-char evil-insert-state-map)
  (bind-key "C-f" 'evil-forward-char evil-insert-state-map)
  (bind-key "C-b" 'evil-backward-char evil-normal-state-map)
  (bind-key "C-b" 'evil-backward-char evil-insert-state-map)
  (bind-key "C-b" 'evil-backward-char evil-visual-state-map)
  (bind-key "C-d" 'evil-delete-char evil-normal-state-map)
  (bind-key "C-d" 'evil-delete-char evil-insert-state-map)
  (bind-key "C-d" 'evil-delete-char evil-visual-state-map)
  (bind-key "C-n" 'evil-next-line evil-normal-state-map)
  (bind-key "C-n" 'evil-next-line evil-insert-state-map)
  (bind-key "C-n" 'evil-next-line evil-visual-state-map)
  (bind-key "C-p" 'evil-previous-line evil-normal-state-map)
  (bind-key "C-p" 'evil-previous-line evil-insert-state-map)
  (bind-key "C-p" 'evil-previous-line evil-visual-state-map)
  (bind-key "C-w" 'evil-delete-backward-word evil-normal-state-map)
  (bind-key "C-w" 'evil-delete-backward-word evil-insert-state-map)
  (bind-key "C-w" 'evil-delete-backward-word evil-visual-state-map)
  (bind-key "C-k" 'kill-line evil-normal-state-map)
  (bind-key "C-k" 'kill-line evil-insert-state-map)
  (bind-key "C-k" 'kill-line evil-visual-state-map)
  (bind-key "Q" 'call-last-kbd-macro evil-normal-state-map)
  (bind-key "Q" 'call-last-kbd-macro evil-visual-state-map)

  ;; smarter c-a
  (bind-key "C-a" 't/smart-beginning-of-line evil-normal-state-map)
  (bind-key "C-a" 't/smart-beginning-of-line evil-insert-state-map)
  (bind-key "C-a" 't/smart-beginning-of-line evil-visual-state-map)
  (bind-key "C-a" 't/smart-beginning-of-line evil-motion-state-map)

  ;; cycle after pasting with p
  (bind-key "C-y" 'evil-paste-pop evil-normal-state-map)
  (bind-key "C-y" 'evil-paste-pop evil-visual-state-map)
  (bind-key "C-S-y" (lambda () (interactive) (evil-paste-pop -1)) evil-normal-state-map)
  (bind-key "C-S-y" (lambda () (interactive) (evil-paste-pop -1)) evil-visual-state-map)
  ;; show kill ring when not in insert mode, where c-y repeats text from above line
  (bind-key "M-y" 'helm-show-kill-ring evil-normal-state-map)
  (bind-key "M-y" 'helm-show-kill-ring evil-insert-state-map)
  (bind-key "M-y" 'helm-show-kill-ring evil-visual-state-map)
  (when (boundp 'evil-hybrid-state-map)
    (bind-key "M-y" 'helm-show-kill-ring evil-hybrid-state-map))

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
