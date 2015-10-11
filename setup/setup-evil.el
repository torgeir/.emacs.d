(use-package evil
  :init
  (progn
    (setq evil-default-state 'normal
          evil-search-module 'evil-search
          evil-emacs-state-cursor  '("orange" box)
          evil-normal-state-cursor '("gray" box)
          evil-visual-state-cursor '("gray" box)
          evil-insert-state-cursor '("gray" bar)
          evil-motion-state-cursor '("gray" box))

    (setq evil-toggle-key "C-'")
    (bind-key "C-'" 'evil-mode))

  :config
  (progn
    (bind-key "C-h" 'evil-window-left evil-normal-state-map)
    (bind-key "C-j" 'evil-window-down evil-normal-state-map)
    (bind-key "C-k" 'evil-window-up evil-normal-state-map)
    (bind-key "C-l" 'evil-window-right evil-normal-state-map)

    (dolist (mode-map '((help-mode . emacs)
                        (compilation-mode . emacs)
                        (special-mode . emacs)
                        (diff-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))))

(use-package evil-escape
  :init
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-paredit
  :config
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))

(use-package evil-numbers
  :config
  (evil-leader/set-key
    "+" 'evil-numbers/inc-at-pt
    "-" 'evil-numbers/dec-at-pt))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-leader
  :commands (evil-leader-mode)
  :init (global-evil-leader-mode)
  :config
  (progn

    (evil-leader/set-leader "SPC")

    ;; clear highlights
    (evil-leader/set-key
      "SPC" (lambda () (interactive)
              (evil-ex-nohighlight)
              (highlight-symbol-remove-all)))

    (evil-leader/set-key
      "xb" 'helm-buffers-list
      "xk" 'ido-kill-buffer
      "xm" 'helm-M-x
      "xM" 'smex-major-mode-commands)

    (evil-leader/set-key
      "xtc" 'transpose-chars
      "xtw" 'transpose-words
      "xtl" 'transpose-lines
      "xtf" 'transpose-frame)

    (evil-leader/set-key
      "re" 'evil-show-registers)

    (evil-leader/set-key
      "bw" 'save-buffer
      "bq" 'kill-buffer)

    ;; files
    (evil-leader/set-key
      "ff" 'helm-do-ag-this-file
      "fo" 'open-in-desktop
      "fr" 'revert-buffer
      "fd" 'delete-current-buffer-file)

    ;; help
    (evil-leader/set-key
      "hav" 'apropos-variable
      "ham" 'apropos-mode
      "had" 'apropos-documentation
      "hb" 'helm-descbinds
      "hf" 'describe-function
      "hk" 'describe-key-briefly
      "hK" 'describe-key
      "hv" 'describe-variable
      "hm" 'describe-mode
      "hM" 'describe-minor-mode
      "hp" 'describe-package)

    ;; errors
    (evil-leader/set-key
      "ec" 'flycheck-clear-errors
      "ep" 'flycheck-previous-error
      "en" 'flycheck-next-error
      "el" 'flycheck-list-errors
      "ev" 'flycheck-verify-setup
      "et" 'flycheck-mode)

    (defun spacemacs/evil-yank-to-end-of-line ()
      "Yank from point to end of line."
      (interactive)
      (evil-yank (point) (point-at-eol)))

    (bind-key (kbd "Y") 'spacemacs/evil-yank-to-end-of-line evil-normal-state-map)
    (bind-key (kbd "Y") 'spacemacs/evil-yank-to-end-of-line evil-motion-state-map)))

(use-package evil-surround
  :init
  (global-evil-surround-mode 1))

(use-package evil-visualstar
  :defer t)

(use-package evil-nerd-commenter
  :config
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cc" 'evilnc-comment-or-uncomment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cy" 'evilnc-copy-and-comment-lines))

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

(evil-mode 1)

(provide 'setup-evil)
