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

    (bind-key "C-'" 'evil-mode))

  :config
  (progn
    (bind-key "C-h" 'evil-window-left evil-normal-state-map)
    (bind-key "C-j" 'evil-window-down evil-normal-state-map)
    (bind-key "C-k" 'evil-window-up evil-normal-state-map)
    (bind-key "C-l" 'evil-window-right evil-normal-state-map)

    (dolist (mode-map '((help-mode . emacs)
                        (fundamental-mode . emacs)
                        (diff-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

))

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

               (evil-leader/set-key
                 "xm" 'smex
                 "xM" 'smex-major-mode-commands)

               (evil-leader/set-key
                 "re" 'evil-show-registers)

               (evil-leader/set-key
                 "bw" 'save-buffer
                 "bq" 'kill-buffer)

               (defun spacemacs/evil-yank-to-end-of-line ()
                 "Yank from point to end of line."
                 (interactive)
                 (evil-yank (point) (point-at-eol)))

               (bind-key (kbd "Y") 'spacemacs/evil-yank-to-end-of-line evil-normal-state-map)
               (bind-key (kbd "Y") 'spacemacs/evil-yank-to-end-of-line evil-motion-state-map)))

(use-package evil-surround
             :init
             (global-evil-surround-mode 1))

(use-package evil-visualstar)

(use-package evil-nerd-commenter
             :config
             (evil-leader/set-key
               "cc" 'evilnc-comment-or-uncomment-lines
               "cp" 'evilnc-comment-or-uncomment-paragraphs
               "cy" 'evilnc-copy-and-comment-lines))

(evil-mode 1)

(provide 'setup-evil)
