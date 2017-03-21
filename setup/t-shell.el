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
  (defun t/ansi-term-handle-close ()
    "Close current term buffer when `exit' or c-d from term buffer."
    (when (ignore-errors (get-buffer-process (current-buffer)))
      (set-process-sentinel
       (get-buffer-process (current-buffer))
       (lambda (proc change)
         (when (string-match "\\(finished\\|exited\\)" change)
           (kill-buffer (process-buffer proc)))))))

  (add-hook 'term-mode-hook 't/ansi-term-handle-close)

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
  (add-hook 'term-mode-hook (lambda () (setq yas-dont-activate t))))

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
        eshell-hist-ignoredups t
        eshell-save-history-on-exit t
        eshell-list-files-after-cd t
        eshell-banner-message ""
        eshell-error-if-no-glob t
        eshell-visual-commands '("less" "tmux" "top" "bash" "vim")
        eshell-visual-subcommands '(("git" "log" "df" "diff" "show"))
        eshell-term-name "eterm-color")  ; announce terminal
  (require 'em-alias)
  (eshell/alias "gs" "magit-status")
  (eshell/alias "gd" "magit-diff-unstaged")
  (eshell/alias "gds" "magit-diff-staged")
  (eshell/alias "emacs" "find-file $1")
  (eshell/alias "e" "find-file $1")
  (eshell/alias "esudo" "find-file /sudo::/$1")
  (eshell/alias "sudo" "*sudo $*")
  (eshell/alias "d" "dired $1")
  (eshell/alias "j" "z")
  (eshell/alias "md" "mkdir $1; cd $1")
  (eshell/alias "l" "ls -la"))

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil))

(defun eshell/d (&rest args)
  (dired (pop args) "."))

(t/eshell-init)

(defun t/eshell-buffer-id ()
  "Next eshell buffer id."
  (s-replace-all '(("*eshell*" . "")
                   ("<" . "")
                   (">" . ""))
                 (generate-new-buffer-name "*eshell*")))

(defun t/eshell-path-of-current-dir ()
  (file-name-directory (or (buffer-file-name) default-directory)))

(defun t/eshell ()
  "Start, or switch to, `eshell' in the current working directory."
  (interactive)
  (let ((path (t/eshell-path-of-current-dir))
        (hasfile (not (eq (buffer-file-name) nil))))
    (eshell (t/eshell-buffer-id))
    (when (and hasfile (eq eshell-process-list nil))
      (t/eshell-buffer-init path))))

(defun t/eshell-buffer-init (path)
  (eshell/cd path)
  (insert (propertize "ls" 'face 'font-lock-comment-face))
  (eshell-send-input)
  (setenv "PAGER" "cat"))

(defun t/eshell-clear ()
  "Clear the eshell buffer."
  (interactive)
  (let* ((inhibit-read-only t)
         (last (and (eolp) (eshell-get-old-input))))
    (erase-buffer)
    (eshell-reset)
    (when last
      (insert last))
    (evil-cp-append 1)))

(defun t/eshell-quit-or-delete-char ()
  (interactive)
  (if (and (eolp)
           (looking-back eshell-prompt-regexp))
      (eshell-life-is-too-much)
    (delete-forward-char 1)))

(add-hook 'eshell-mode-hook
          (lambda ()
            (paredit-mode 1)
            (bind-key "C-l" 't/eshell-clear eshell-mode-map)
            (bind-key "C-a" 'eshell-bol eshell-mode-map)
            (bind-key "C-a" 'eshell-bol evil-insert-state-local-map)
            (bind-key "C-a" 'eshell-bol evil-normal-state-local-map)
            (bind-key "C-d" 't/eshell-quit-or-delete-char eshell-mode-map)
            (bind-key "C-d" 't/eshell-quit-or-delete-char evil-insert-state-local-map)
            (bind-key "C-d" 't/eshell-quit-or-delete-char evil-normal-state-local-map)
            (progn ;; helm for history
              (setq eshell-cmpl-ignore-case t)
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
            (progn
              (defun t/eshell-kill-input--go-to-eol ()
                "Go to end of line before killing input"
                (end-of-line))
              (advice-add 'eshell-kill-input :before #'t/eshell-kill-input--go-to-eol))
            (bind-key "C-u" 'eshell-kill-input eshell-mode-map)
            ;; C-c c-d sends exit
            (bind-key "C-c C-u" 'universal-argument eshell-mode-map)))

;; fix wierd prompts
(add-to-list
 'eshell-preoutput-filter-functions
 (lambda (output)
   (replace-regexp-in-string "\\[[0-9]+[G-K]" "" output)))

(use-package eshell-z
  :defer t
  :init
  (with-eval-after-load 'eshell
    (require 'eshell-z)))

(progn
  (defun curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
        (if (> (length git-output) 0)
            (concat " " (substring git-output 0 -1))
          "(no branch)"))))

  (defun pwd-replace-home (pwd)
    "Replace home in PWD with tilde (~) character."
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
        pwd)))

  (defun pwd-shorten-dirs (pwd)
    "Shorten all directory names in PWD except the last two."
    (let ((p-lst (split-string pwd "/")))
      (if (> (length p-lst) 2)
          (concat
           (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                 (substring elm 0 1)))
                      (butlast p-lst 2)
                      "/")
           "/"
           (mapconcat (lambda (elm) elm)
                      (last p-lst 2)
                      "/"))
        pwd)))  ;; Otherwise, we just return the PWD

  (defun split-directory-prompt (directory)
    (if (string-match-p ".*/.*" directory)
        (list (file-name-directory directory) (file-name-base directory))
      (list "" directory)))

  (setq eshell-prompt-function
        (lambda ()
          (let* ((directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
                 (parent (car directory))
                 (name (cadr directory))
                 (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))

            (concat
             (propertize parent 'face `font-lock-builtin-face)
             (propertize name 'face `font-lock-constant-face)
             (propertize branch 'face `font-lock-string-face)
             (propertize " $" 'face `font-lock-comment-face)
             (propertize " " 'face `font-lock-preprocessor-face))))))


(provide 't-shell)
