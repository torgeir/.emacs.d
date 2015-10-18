(bind-key [remap goto-line] 'goto-line-with-feedback) ; m-g m-g
(bind-key "M-j" 'join-lines)
(bind-key "M-p" 'backward-paragraph)
(bind-key "M-n" 'forward-paragraph)

(bind-key "C-o" 'open-line-below)
(bind-key "C-w" 'backward-kill-word)
(bind-key "C-w" 'backward-kill-word minibuffer-local-map) ; minibuffer
(bind-key "C-M-q" 'quit-other-window)

(bind-key "C-c d" 'duplicate-current-line-or-region)
(bind-key "C-c n" 'cleanup-buffer-whitespace-and-indent)
(bind-key "C-c w o" 'delete-other-windows)
(bind-key "C-c f o" 'open-in-desktop)
(bind-key "C-c C-r" 'revert-buffer)
(bind-key "C-c C-d" 'delete-current-buffer-file)
(bind-key "C-c C-e" 'eval-and-replace)
(bind-key "C-c C-y" #'insert-char-above-the-cursor)

(bind-key "C-x C-g" 'ffap)
(bind-key "C-x C-k" 'kill-region)
(bind-key "C-x C-k" 'isearch-delete-me isearch-mode-map)

(defun config-reload () (interactive) (load-file "~/.emacs.d/init.el"))
(defun config-edit-init () (interactive) (find-file "~/.emacs.d/init.el"))
(defun config-edit-sane-defaults () (interactive) (find-file "~/.emacs.d/setup/sane-defaults.el"))
(defun config-edit-defuns () (interactive) (find-file "~/.emacs.d/setup/defuns.el"))
(defun config-edit-keys () (interactive) (find-file "~/.emacs.d/setup/keys.el"))
(defun config-edit-mac () (interactive) (find-file "~/.emacs.d/setup/mac.el"))
(defun config-edit-langs () (interactive) (find-file "~/.emacs.d/setup/langs.el"))
(defun config-edit-snippets () (interactive) (find-file "~/.emacs.d/snippets/"))

(declare-prefix "C" "Edit config"
                "r" 'config-reload
                "i" 'config-edit-init
                "e" 'config-edit-sane-defaults
                "d" 'config-edit-defuns
                "k" 'config-edit-keys
                "m" 'config-edit-mac
                "l" 'config-edit-langs
                "s" 'config-edit-snippets)

;; lisp-friendly
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-all-abbrevs
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-list
        try-expand-line))

(bind-key "C-." 'hippie-expand-no-case-fold)
(bind-key "C-," 'completion-at-point)
(bind-key "C-S-." 'hippie-expand-lines)

(provide 'keys)
