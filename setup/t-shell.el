(use-package bash-completion
  :commands bash-completion-dynamic-complete
  :init
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete))

(progn
  ;; C-d to kill buffer if process is dead
  (defun t/comint-delchar-or-eof-or-kill-buffer (arg)
    (interactive "p")
    (if (null (get-buffer-process (current-buffer)))
        (kill-buffer)
      (comint-delchar-or-maybe-eof arg)))

  (add-hook 'shell-mode-hook
            (lambda ()
              (bind-key "C-d" 't/comint-delchar-or-eof-or-kill-buffer shell-mode-map))))

(progn
  ;; ansi-term
  (defun ansi-term-handle-close ()
    "Close current term buffer when `exit' or c-d from term buffer."
    (when (ignore-errors (get-buffer-process (current-buffer)))
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)" change)
           (kill-buffer (process-buffer proc))
           (condition-case nil
               (delete-window)
             (error nil)))))))

  ;; exit for realz
  (add-hook 'term-mode-hook 'ansi-term-handle-close)

  (add-hook 'term-mode-hook
            (lambda ()
              (bind-key "C-d" 't/volatile-kill-buffer evil-insert-state-local-map)
              (bind-key "C-d" 't/volatile-kill-buffer evil-normal-state-local-map)))

  ;; stfu
  (defconst t-term-name "/bin/zsh")
  (defadvice ansi-term (before force-bash) (interactive (list t-term-name)))
  (ad-activate 'ansi-term)

  (defun t/shell ()
    "Start a shell"
    (interactive)
    (ansi-term t-term-name))

  ;; fix tab-completion in ansi-term
  (add-hook 'term-mode-hook (lambda () (setq yas-dont-activate t)))

  ;; ansi colors in shell
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(defun t/eshell-init-smart ()
  "Init smart eshell"
  (require 'em-smart)
  (eval-after-load 'esh-module '(add-to-list 'eshell-modules-list 'eshell-smart))
  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil
        eshell-smart-space-goes-to-end t))

(defun t/eshell-init ()
  "Init eshell"
  (t/eshell-init-smart)
  (setq eshell-history-size 10000
        eshell-save-history-on-exit t
        eshell-visual-commands '("less" "tmux" "top" "bash")
        eshell-visual-subcommands '(("git" "log" "df" "diff" "show"))
        eshell-term-name "eterm-color")) ; announce terminal

(t/eshell-init)

(defun t/eshell-buffer-id ()
  "Next eshell buffer id."
  (s-replace-all '(("*eshell*" . "")
                   ("<" . "")
                   (">" . ""))
                 (generate-new-buffer-name "*eshell*")))

(defun t/eshell ()
  "Start, or switch to, `eshell' in the current working directory."
  (interactive)
  (let ((path (file-name-directory (or (buffer-file-name) default-directory)))
        (hasfile (not (eq (buffer-file-name) nil))))
    (eshell (t/eshell-buffer-id))
    (if (and hasfile (eq eshell-process-list nil))
        (progn
          (eshell/cd path)
          (eshell-reset)))))

(defun t/eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (bind-key "C-l" 't/eshell-clear eshell-mode-map)
            (bind-key "C-a" 'eshell-bol eshell-mode-map)
            (bind-key "C-a" 'eshell-bol evil-insert-state-local-map)
            (bind-key "C-a" 'eshell-bol evil-normal-state-local-map)
            (bind-key "C-d" 'eshell-life-is-too-much eshell-mode-map)
            (bind-key "C-d" 'eshell-life-is-too-much evil-insert-state-local-map)
            (bind-key "C-d" 'eshell-life-is-too-much evil-normal-state-local-map)
            (defun t/eshell-kill-input--go-to-eol ()
              "Go to end of line before killing input"
              (end-of-line))
            (advice-add 'eshell-kill-input :before #'t/eshell-kill-input--go-to-eol)
            (bind-key "C-u" 'eshell-kill-input eshell-mode-map)
            (bind-key "C-c C-u" 'universal-argument eshell-mode-map)))

(provide 't-shell)
