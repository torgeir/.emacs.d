(use-package bash-completion
  :commands (bash-completion-dynamic-complete)
  :config
  (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
  (add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete))

;; C-d to kill buffer if process is dead
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(defun ansi-term-handle-close ()
  "Close current term buffer when `exit' or c-d from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc))
                              (delete-window))))))

(add-hook 'term-mode-hook 'ansi-term-handle-close)

;; tab-completion
(use-package shell-command
  :defer t
  :config
  (shell-command-completion-mode 1))

;; fix tab-completion in ansi-term
(add-hook 'term-mode-hook (lambda () (setq yas-dont-activate t)))

;; ansi colors in shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun t/eshell ()
  "Start, or switch to, `eshell' in the current working directory."
  (interactive)
  (let ((path (file-name-directory
               (or (buffer-file-name) "~/")))
        (hasfile (not (eq (buffer-file-name) nil))))
    (eshell)
    (if (and hasfile (eq eshell-process-list nil))
        (progn
          (eshell/cd path)
          (eshell-reset)))))

(provide 'setup-shell)
