(use-package evil
  :init
  (progn
    (setq evil-default-state 'normal
          evil-search-module 'evil-search
          evil-emacs-state-cursor  '("orange" box)
          evil-normal-state-cursor '("green" box)
          evil-visual-state-cursor '("green" box)
          evil-insert-state-cursor '("green" bar)
          evil-motion-state-cursor '("green" box))

    (setq evil-toggle-key "C-'")
    (bind-key "C-'" 'evil-mode))

  :config
  (progn
    (dolist (mode-map '((help-mode . emacs)
                        (compilation-mode . emacs)
                        (special-mode . emacs)
                        (flycheck-error-list-mode . emacs)
                        (diff-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-numbers
  :config
  (evil-leader/set-key
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (progn

    (evil-leader/set-leader leader)

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

(use-package evil-surround
  :init
  (global-evil-surround-mode 1)
  :config
  ;; the opposite of vim, like spacemacs
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)
  (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region))

(use-package evil-paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
  ;; evil-surround support
  (add-to-list 'evil-surround-operator-alist '(evil-paredit-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-paredit-delete . delete)))

(use-package evil-visualstar
  :defer t
  :config
  (global-evil-visualstar-mode))

(use-package evil-nerd-commenter
  :config
  (declare-prefix "c" "Comments"
                  "i" 'evilnc-comment-or-uncomment-lines
                  "c" 'evilnc-comment-or-uncomment-lines
                  "p" 'evilnc-comment-or-uncomment-paragraphs
                  "ap" 'evilnc-comment-or-uncomment-paragraphs
                  "y" 'evilnc-copy-and-comment-lines))

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
(bind-key "C-w" 'evil-delete evil-normal-state-map)
(bind-key "C-w" 'evil-delete evil-insert-state-map)
(bind-key "C-w" 'evil-delete evil-visual-state-map)
(bind-key "C-k" 'kill-line evil-normal-state-map)
(bind-key "C-k" 'kill-line evil-insert-state-map)
(bind-key "C-k" 'kill-line evil-visual-state-map)
(bind-key "Q" 'call-last-kbd-macro evil-normal-state-map)
(bind-key "Q" 'call-last-kbd-macro evil-visual-state-map)

;; smarter c-a
(bind-key "C-a" 'smart-beginning-of-line evil-normal-state-map)
(bind-key "C-a" 'smart-beginning-of-line evil-insert-state-map)
(bind-key "C-a" 'smart-beginning-of-line evil-visual-state-map)
(bind-key "C-a" 'smart-beginning-of-line evil-motion-state-map)

;; cycle after pasting with p
(bind-key "\M-y" 'evil-paste-pop evil-normal-state-map)
(bind-key "\M-y" 'evil-paste-pop evil-visual-state-map)
;; show kill ring when not in insert mode, where c-y repeats text from above line
(bind-key "C-y" 'helm-show-kill-ring evil-normal-state-map)
(bind-key "C-y" 'helm-show-kill-ring evil-visual-state-map)

;;; esc ought to quit
(bind-key [escape] 'keyboard-quit evil-normal-state-map)
(bind-key [escape] 'keyboard-quit evil-visual-state-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-ns-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-completion-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-must-match-map)
(bind-key [escape] 'minibuffer-keyboard-quit minibuffer-local-isearch-map)

(declare-prefix "E" "Emacs"
                "t" 'load-theme
                "l" 'linum-mode
                "b" 'fancy-battery-mode)

(declare-prefix "Ep" "Packages"
                "i" 'package-install)

(declare-prefix "b" "Buffers"
                "w" 'save-buffer
                "k" 'kill-buffer
                "K" 'kill-other-buffers
                "b" 'helm-buffers-list
                "R" 'revert-buffer)

(declare-prefix "x" "Text manipulation"
                "f" 'ido-find-file
                "k" 'ido-kill-buffer
                "m" 'helm-M-x
                "x" 'smex-major-mode-commands)

(declare-prefix "xt" "Transpose"
                "c" 'transpose-chars
                "w" 'transpose-words
                "l" 'transpose-lines
                "f" 'transpose-frame)

(declare-prefix "r" "Registers"
                "e" 'evil-show-registers)

(declare-prefix "f" "Files"
                "f" 'helm-find-files
                "k" 'kill-buffer
                "s" 'save-buffer
                "S" 'save-some-buffers
                "o" 'open-in-desktop
                "R" 'revert-buffer
                "r" 'helm-recentf
                "d" 'delete-current-buffer-file)

(declare-prefix "h" "Help"
                "b" 'helm-descbinds
                "f" 'describe-function
                "k" 'describe-key-briefly
                "K" 'describe-key
                "v" 'describe-variable
                "m" 'describe-mode
                "M" 'describe-minor-mode
                "p" 'describe-package)

(declare-prefix "ha" "Help apropos"
                "v" 'apropos-variable
                "m" 'apropos-mode
                "d" 'apropos-documentation)

(declare-prefix "e" "Errors"
                "c" 'flycheck-clear
                "p" 'flycheck-previous-error
                "n" 'flycheck-next-error
                "l" 'flycheck-list-errors
                "v" 'flycheck-verify-setup
                "t" 'flycheck-mode)

(declare-prefix "w" "Windows"
                "n" 'make-frame-command
                "c" 'delete-frame-or-hide-last-remaining-frame
                "h" 'evil-window-left
                "j" 'evil-window-down
                "k" 'evil-window-up
                "l" 'evil-window-right)

(declare-prefix "s" "Search"
                "b" 'helm-ag-buffers
                "f" 'helm-ag-this-file
                "p" 'helm-projectile-ag)

(declare-prefix "ss" "Helm Swoop"
                "s" 'helm-swoop
                "m" 'helm-multi-swoop
                "a" 'helm-multi-swoop-all)

(declare-prefix "sw" "Search Web"
                "g" 'helm-google-suggest
                "w" 'helm-wikipedia-suggest)

(evil-mode 1)

(provide 'setup-evil)
