(defconst t-evil-cursor-color-emacs "dark orange")
(defconst t-evil-cursor-color-evil "green2")

(t/use-package evil
  :only-standalone t)

(t/use-package evil-anzu
  :only-standalone t
  :init
  (progn
    (setq anzu-cons-mode-line-p nil
          anzu-minimum-input-length 1
          anzu-search-threshold 100)))

(t/use-package evil-escape
  :only-standalone t
  :after evil
  :init
  (progn
    (setq-default evil-escape-key-sequence "jk"
                  evil-escape-delay 0.08)
    (with-eval-after-load 'evil-escape
      (evil-escape-mode))))

(t/use-package evil-leader
  :after evil
  :config
  (progn
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader t-leader)
    (evil-mode nil)
    (global-evil-leader-mode)
    (evil-mode 1)

    (defun clear-all-highlights ()
      (interactive)
      (evil-ex-nohighlight)
      (when (fboundp 'evil-search-highlight-persist-remove-all)
        (evil-search-highlight-persist-remove-all))
      (highlight-symbol-remove-all))

    (t/declare-prefix "s" "Search"
                      "c" 'clear-all-highlights)

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

(t/use-package evil-surround
  :only-standalone t
  :after evil
  :config
  (progn
    (global-evil-surround-mode 1)
    ;; the opposite of vim, like spacemacs
    (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
    (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)))


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
  (progn (setq evil-cleverparens-use-additional-bindings nil)))

(t/use-package evil-multiedit
  :commands evil-multiedit-match-symbol-and-next
  :init
  (progn
    (setq evil-multiedit-follow-matches t)
    (bind-key "M-d" 'evil-multiedit-match-symbol-and-next evil-normal-state-map)
    (with-eval-after-load 'evil-multiedit
      (bind-key "M-j" 'evil-multiedit-toggle-or-restrict-region evil-multiedit-state-map)
      (bind-key "M-k" 'evil-multiedit-toggle-or-restrict-region evil-multiedit-state-map)
      (bind-key "M-j" 'evil-multiedit-toggle-or-restrict-region evil-multiedit-insert-state-map)
      (bind-key "M-k" 'evil-multiedit-toggle-or-restrict-region evil-multiedit-insert-state-map)))
  :config
  (progn
    (evil-multiedit-default-keybinds)
    (unbind-key "M-d" evil-insert-state-map)
    
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
  (setq evil-default-state 'normal
        evil-insert-skip-empty-lines t
        evil-search-module 'evil-search))

(defun t-evil/funcs ()
  (setq                                 ; initial colors
   evil-emacs-state-cursor `(,t-evil-cursor-color-emacs box)
   evil-normal-state-cursor `(,t-evil-cursor-color-evil box)
   evil-visual-state-cursor `(,t-evil-cursor-color-evil hollow))

  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (add-hook 'org-capture-mode-hook 'evil-insert-state)

  (defun t/evil-update-cursor-color ()
    (let* ((colors `((emacs . ,t-evil-cursor-color-emacs)
                     (normal . ,t-evil-cursor-color-evil)
                     (visual . ,t-evil-cursor-color-evil)
                     (motion . ,t-evil-cursor-color-evil)
                     (insert . ,t-evil-cursor-color-evil)))
           (state-color (assoc evil-state colors)))
      (set-cursor-color (if state-color (cdr state-color) t-evil-cursor-color-emacs))))

  (defun t/toggle-evil-local-mode ()
    "Toggles evil-local-mode and updates the cursor. The `evil-*-state-cursor's seem to work by default, but not when toggling evil-local-mode (in emacsclients) off and on, so help it."
    (interactive)
    (evil-local-mode (if evil-local-mode 0 1))
    (t/evil-update-cursor-color)))

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
      "Run the `+evil-esc-hook'."
      (cond ((minibuffer-window-active-p (minibuffer-window))
             ;; quit the minibuffer if open.
             (abort-recursive-edit))
            ((evil-ex-hl-active-p 'evil-ex-search)
             ;; disable ex search buffer highlights.
             (evil-ex-nohighlight))
            (t
             ;; Run all escape hooks. If any returns
             ;; non-nil, then stop there.
             (run-hook-with-args-until-success '+evil-esc-hook))))

    (defvar t-evil-major-modes '(compilation-mode
                                 special-mode
                                 calendar-mode
                                 git-rebase-mode
                                 flycheck-error-list-mode
                                 diff-mode
                                 cider-stacktrace-mode
                                 cider-docview-mode)
      "Major modes that should trigger evil emacs state when changed to.")
    (add-hook 'after-change-major-mode-hook
              (lambda ()
                (when (member major-mode t-evil-major-modes)
                  (evil-emacs-state)))))

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
