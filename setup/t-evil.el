(setq t-evil-cursor-color-emacs "dark orange"
      t-evil-cursor-color-evil "green2")

(defun t/evil-update-cursor-color ()
  (let* ((colors `((emacs . ,t-evil-cursor-color-emacs)
                   (normal . ,t-evil-cursor-color-evil)
                   (visual . ,t-evil-cursor-color-evil)
                   (motion . ,t-evil-cursor-color-evil)
                   (insert . ,t-evil-cursor-color-evil)))
         (state-color (assoc evil-state colors)))
    (set-cursor-color (if state-color (cdr state-color) t-evil-cursor-color-emacs))))

(use-package evil
  :init
  (setq evil-default-state 'normal
        evil-search-module 'evil-search
        ;; initial colors
        evil-emacs-state-cursor `(,t-evil-cursor-color-emacs box)
        evil-normal-state-cursor `(,t-evil-cursor-color-evil box)
        evil-visual-state-cursor `(,t-evil-cursor-color-evil hollow))

  (defun t/toggle-evil-local-mode ()
    "Toggles evil-local-mode and updates the cursor. The `evil-*-state-cursor's seem to work by default, but not when toggling evil-local-mode (in emacsclients) off and on, so help it."
    (interactive)
    (evil-local-mode (if evil-local-mode 0 1))
    (t/evil-update-cursor-color))

  (if is-mac
      (bind-key "C-'" 't/toggle-evil-local-mode)
    (bind-key "C-|" 't/toggle-evil-local-mode))

  :config
  (progn
    (defvar t-evil-major-modes '(help-mode
                                 compilation-mode
                                 special-mode
                                 calendar-mode
                                 git-rebase-mode
                                 flycheck-error-list-mode
                                 diff-mode
                                 cider-stacktrace-mode
                                 cider-docview-mode)
      "major modes that should trigger evil emacs state when changed to")
    (add-hook 'after-change-major-mode-hook
              (lambda ()
                (when (member major-mode t-evil-major-modes)
                  (evil-emacs-state))))))

(use-package evil-anzu
  :after evil)

(use-package evil-escape
  :after evil
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.08)
  (evil-escape-mode))

(use-package evil-leader
  :after evil
  :config
  (progn
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader *user-leader*)
    (evil-mode nil)
    (global-evil-leader-mode)
    (evil-mode 1)

    (defun clear-all-highlights ()
      (interactive)
      (evil-ex-nohighlight)
      (highlight-symbol-remove-all))

    ;; clear highlights with leader leader
    (evil-leader/set-key *user-leader* 'clear-all-highlights)

    (defun spacemacs/evil-yank-to-end-of-line ()
      "Yank from point to end of line."
      (interactive)
      (evil-yank (point) (point-at-eol)))

    (bind-key (kbd "Y") 'spacemacs/evil-yank-to-end-of-line evil-normal-state-map)
    (bind-key (kbd "Y") 'spacemacs/evil-yank-to-end-of-line evil-motion-state-map)))

(use-package evil-numbers
  :commands (evil-numbers/inc-at-pt
             evil-numbers/dec-at-pt)
  :init
  (evil-leader/set-key
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt))

(use-package evil-matchit
  :after evil
  :commands evilmi-jump-items
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  ;; the opposite of vim, like spacemacs
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))

(use-package evil-visualstar
  :commands (evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (define-key evil-visual-state-map (kbd "*")
    'evil-visualstar/begin-search-forward)
  (define-key evil-visual-state-map (kbd "#")
    'evil-visualstar/begin-search-backward))

(use-package evil-paredit
  :after evil
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
  (add-hook 'clojure-mode-hook 'evil-paredit-mode)
  ;; evil-surround support
  (add-to-list 'evil-surround-operator-alist '(evil-paredit-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-paredit-delete . delete)))

(use-package evil-nerd-commenter
  :defer 2
  :init
  (t/declare-prefix "c" "Comment/Complete"
                    "c" 'evilnc-comment-or-uncomment-lines
                    "p" 'evilnc-comment-or-uncomment-paragraphs
                    "ap" 'evilnc-comment-or-uncomment-paragraphs
                    "y" 'evilnc-copy-and-comment-lines
                    "w" 'evil-complete-next
                    "W" 'evil-complete-previous))


;; motions keys for help buffers
(evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
(evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
(evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
(evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
(evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
(evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

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

;;; esc ought to quit
(bind-key [escape] 'keyboard-quit evil-normal-state-map)
(bind-key [escape] 'keyboard-quit evil-visual-state-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

(provide 't-evil)