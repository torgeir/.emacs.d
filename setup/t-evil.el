(defconst t-evil-cursor-color-emacs "dark orange")
(defconst t-evil-cursor-color-evil "green2")

(t/use-package evil
  :only-standalone t)

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
      (evil-search-highlight-persist-remove-all)
      (highlight-symbol-remove-all))

    ;; clear highlights with leader leader
    (evil-leader/set-key t-leader 'clear-all-highlights)

    (defun spacemacs/evil-yank-to-end-of-line ()
      "Yank from point to end of line."
      (interactive)
      (evil-yank (point) (point-at-eol)))

    (bind-key "Y" 'spacemacs/evil-yank-to-end-of-line evil-normal-state-map)
    (bind-key "Y" 'spacemacs/evil-yank-to-end-of-line evil-motion-state-map)))

(t/use-package evil-numbers
  :only-standalone t
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)
  :init
  (progn
    (evil-leader/set-key
      "+" 'evil-numbers/inc-at-pt
      "-" 'evil-numbers/dec-at-pt)))

(t/use-package evil-matchit
  :only-standalone t
  :commands evilmi-jump-items
  :config
  (progn
    (global-evil-matchit-mode 1)))

(t/use-package evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

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
  :config
  (progn
    (unbind-key "M-d" evil-cleverparens-mode-map)))

(t/use-package org-evil
  :after evil)

(t/use-package evil-commentary
  :after evil)

(defun t-evil/vars ()
  (setq evil-default-state 'normal
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
    (defvar t-evil-major-modes '(;;help-mode
                                 compilation-mode
                                 special-mode
                                 calendar-mode
                                 git-rebase-mode
                                 flycheck-error-list-mode
                                 diff-mode
                                 ;;term-mode
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

;;; esc ought to quit
  (bind-key [escape] 'keyboard-quit evil-normal-state-map)
  (bind-key [escape] 'keyboard-quit evil-visual-state-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
  (bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

  ;; macro camelCase to snakeCase
  (evil-set-register ?c [?: ?s ?/ ?\\ ?( ?[ ?a ?- ?z ?0 ?- ?9 ?] ?\\ ?) ?\\ ?( ?[ ?A ?- ?Z ?0 ?- ?9 ?] ?\\ ?) ?/ ?\\ ?1 ?_ ?\\ ?l ?\\ ?2 ?/ ?g]))

(provide 't-evil)
