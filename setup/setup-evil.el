(use-package evil
  :init
  (setq cursor-color-emacs "DarkOrange" cursor-color-evil "green3")
  (setq evil-default-state 'normal
        evil-search-module 'evil-search
        evil-emacs-state-cursor  `(,cursor-color-emacs box)
        evil-normal-state-cursor `(,cursor-color-evil box)
        evil-visual-state-cursor `(,cursor-color-evil box)
        evil-insert-state-cursor `(,cursor-color-evil bar)
        evil-motion-state-cursor `(,cursor-color-evil box))

  (if is-mac
    (bind-key "C-'" 'evil-local-mode)
    (bind-key "C-|" 'evil-local-mode))

  :config
  (progn
    (dolist (mode-map '((help-mode . emacs)
                        (compilation-mode . emacs)
                        (special-mode . emacs)
                        (calendar-mode . emacs)
                        (git-rebase-mode . emacs)
                        (flycheck-error-list-mode . emacs)
                        (diff-mode . emacs)
                        (cider-stacktrace-mode . emacs)
                        (cider-docview-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))))

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
    (evil-leader/set-leader leader)
    (evil-mode nil)
    (global-evil-leader-mode)
    (evil-mode 1)

    (defun clear-all-highlights ()
      (interactive)
      (evil-ex-nohighlight)
      (highlight-symbol-remove-all))

    ;; clear highlights with leader leader
    (evil-leader/set-key leader 'clear-all-highlights)

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

(provide 'setup-evil)
