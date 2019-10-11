;;; -*- lexical-binding: t; -*-
(t/use-package bash-completion
  :commands bash-completion-dynamic-complete
  :init
  (progn
    (autoload 'bash-completion-dynamic-complete "bash-completion" "BASH completion hook")
    (t/add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)))

(t/use-package esh-help
  :commands setup-esh-help-eldoc
  :init
  (t/add-hook 'eshell-first-time-mode-hook 'setup-esh-help-eldoc))

(t/use-package esh-autosuggest
  :init
  (t/add-hook 'eshell-mode-hook 'esh-autosuggest-mode)
  :config
  (t/bind-in 'esh-autosuggest-active-map "C-j" 'company-complete-selection))

(t/use-package eshell-z
  :init
  (t/add-hook-defun 'eshell-mode-hook t/eshell-z-hook (require 'eshell-z)))

(defun t-shell/config ()

  (progn
    ;; shell
    (defun t/shell-mode-kill-buffer-on-exit (process state)
      (shell-write-history-on-exit process state)
      (kill-buffer-and-window))

    (defun t/shell-mode-hook ()
      (set-process-sentinel (get-buffer-process (current-buffer))
                            #'t/shell-mode-kill-buffer-on-exit))

    (t/add-hook 'shell-mode-hook #'t/shell-mode-hook))

  (progn
    ;; term
    (setq explicit-shell-file-name "/bin/zsh")

    (defun t/term-mode-hook ()
      (defun term-window-width () 2000)
      (setq truncate-lines t)
      (t/highlight-logging)
      (evil-define-key 'normal term-raw-map "M-:" 'eval-expression)
      (evil-define-key 'normal term-raw-map "p" 'term-paste)
      (evil-define-key 'insert term-raw-map (kbd "<tab>") 'term-send-tab)

      (defun t/term-try-quit ()
        (interactive)
        (t/term-quit-if-finished 'term-send-eof))

      (t/bind-in '(evil-normal-state-local-map evil-insert-state-local-map)
        "C-d" 't/term-try-quit))

    (t/add-hook 'term-mode-hook #'t/term-mode-hook))

  (progn
    ;; ansi-term
    (defun t/ansi-term-mode-hook ()
      "Close current term buffer when `exit' or c-d from term buffer."
      (goto-address-mode)

      (when (ignore-errors (get-buffer-process (current-buffer)))
        (set-process-sentinel
         (get-buffer-process (current-buffer))
         (lambda (proc change)
           (message change)
           (when (string-match "\\(finished\\|exited\\)" change)
             (kill-buffer (process-buffer proc)))))))

    (t/add-hook 'term-mode-hook #'t/ansi-term-mode-hook)

    (defconst t-term-name "/bin/zsh")
    (defadvice ansi-term (before force-bash)
      (interactive (list t-term-name))
      (term-line-mode))
    (ad-activate 'ansi-term)

    (defadvice ansi-term (after always-use-line-mode)
      (term-line-mode))
    (ad-activate 'ansi-term)

    ;; fix tab-completion
    (t/add-hook-setq 'term-mode-hook yas-dont-activate t))

  (progn
    ;; eshell

    (defun t/eshell-init ()
      "Init eshell."
      (t/add-hook-defun 'eshell-first-time-mode-hook t/hook-init-eshell
                        (t/eshell-init-smart)
                        (t/eshell-init-aliases)

                        ;; fix wierd prompts
                        (add-to-list 'eshell-preoutput-filter-functions
                                     (lambda (output)
                                       (replace-regexp-in-string "\\[[0-9]+[G-K]" "" output))))

      (setq eshell-history-size 10000
            eshell-hist-ignoredups t
            eshell-scroll-to-bottom-on-output t
            eshell-save-history-on-exit t
            eshell-list-files-after-cd t
            eshell-banner-message ""
            eshell-error-if-no-glob t
            eshell-visual-commands '("less" "ssh" "tmux" "top" "htop" "bash" "vim")
            eshell-visual-subcommands '(("git" "log" "df" "diff" "show"))
            eshell-term-name "eterm-color"))

    (t/eshell-init)


    (defun t/eshell-init-smart ()
      "Init smart eshell"
      (require 'em-smart)
      (setq eshell-where-to-jump 'begin
            eshell-review-quick-commands nil
            eshell-smart-space-goes-to-end t)
      (eshell-smart-initialize))

    (defun t/eshell-init-aliases ()
      (require 'em-alias)
      (dolist (alias (list
                      '("cleanupdsstore" "find . -name '*.DS_Store' -type f -ls -delete")
                      '("d" "dired $1")
                      '("e" "find-file $1")
                      '("f" "helm-find-files $1")
                      '("p" "helm-projectile")
                      '("emacs" "find-file $1")
                      '("emptytrash" "sudo rm -rfv /Volumes/*/.Trashes; rm -rfv ~/.Trash")
                      '("esudo" "find-file /sudo::/$1")
                      '("flushyosemitedns" "sudo discoveryutil mdnsflushcache;sudo discoveryutil udnsflushcaches")
                      '("gd" "magit-diff-unstaged")
                      '("gds" "magit-diff-staged")
                      '("grep" "grep --color=always $*")
                      '("gs" "magit-status")
                      '("gr" "cd ${git rev-parse --show-toplevel}")
                      '("gadd-origin-pr" "git config --add remote.origin.fetch \"+refs/pull/*/head:refs/remotes/origin/pr/*\"")
                      '("hidedesktop" "defaults write com.apple.finder CreateDesktop -bool false && killall Finder")
                      '("hidehidden" "defaults write com.apple.finder AppleShowAllFiles -boolean false && killall Finder")
                      '("ip" "dig +short myip.opendns.com @resolver1.opendns.com")
                      '("localip" "ipconfig getifaddr en0")
                      '("j" "z $*")
                      '("ll" "ls -laH $*")
                      '("l" "ls -H $*")
                      '("lout" "/System/Library/CoreServices/Menu\\ Extras/User.menu/Contents/Resources/CGSession -suspend")
                      '("md" "mkdir $1; cd $1")
                      '("serve" "python -m SimpleHTTPServer")
                      '("showdesktop" "defaults write com.apple.finder CreateDesktop -bool true && killall Finder")
                      '("showhidden" "defaults write com.apple.finder AppleShowAllFiles -boolean true && killall Finder")
                      '("essh" "cd \"/ssh:$1:~\"")
                      '("sudo" "*sudo $*")))
        (add-to-list 'eshell-command-aliases-list alias)))

    (defun t/eshell-buffer-id ()
      "Next eshell buffer id."
      (concat "*eshell: " (t/eshell-path-of-current-dir) "*"))

    (defun t/eshell-path-of-current-dir ()
      (file-name-directory (or (buffer-file-name) default-directory)))

    (defun t/eshell ()
      "Start, or switch to, `eshell' in the current working directory."
      (interactive)
      (let ((path (t/eshell-path-of-current-dir))
            (hasfile (not (eq (buffer-file-name) nil))))
        (eshell (t/eshell-buffer-id))
        (when (and hasfile (eq eshell-process-list nil))
          (goto-char (point-max))
          (setenv "JAVA_HOME" (s-trim (shell-command-to-string "/usr/libexec/java_home -v 8")))
          (setenv "BOOT_JVM_OPTIONS" "-Djdk.launcher.addmods=java.xml.bind")
          (setenv "PAGER" "cat"))))

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

    (t/add-hook-defun 'eshell-directory-change-hook t/hook-eshell-dir (rename-buffer (t/eshell-buffer-id) t))
    (t/add-hook-defun 'eshell-mode-hook t/hook-eshell
                      (paredit-mode 1)
                      (t/bind-in 'eshell-mode-map
                        "S-<return>" 'newline-and-indent
                        "C-l" 't/eshell-clear
                        "C-a" 'eshell-bol
                        "C-u" 'eshell-kill-input
                        ;; C-c c-d sends exit
                        "C-c C-u" 'universal-argument
                        )
                      (t/bind-in '(eshell-mode-map paredit-mode-map evil-insert-state-local-map evil-normal-state-local-map)
                        "C-a" 'eshell-bol
                        "C-d" 't/eshell-quit-or-delete-char)
                      (progn ;; helm for history
                        (setq eshell-cmpl-ignore-case t)
                        (eshell-cmpl-initialize)
                        (bind-key "C-r" 'helm-eshell-history evil-insert-state-local-map)
                        (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
                        (define-key eshell-mode-map (kbd "M-P") 'helm-eshell-prompts-all)
                        (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history))
                      (progn
                        (defun t/eshell-kill-input--go-to-eol ()
                          "Go to end of line before killing input"
                          (end-of-line))
                        (advice-add 'eshell-kill-input :before #'t/eshell-kill-input--go-to-eol)))

    (progn
      ;; eshell prompt

      (defun curr-dir-git-branch-string (pwd)
        "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
        (interactive)
        (when (and is-mac
                   (eshell-search-path "git")
                   (locate-dominating-file pwd ".git"))
          (let ((git-output (shell-command-to-string (concat "cd " (shell-quote-argument (expand-file-name pwd)) " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
            (if (> (length git-output) 0)
                (concat " " (substring git-output 0 -1))
              " (no branch)"))))

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

      (defun split-directory-prompt (directory short-dir)
        (if (string-match-p ".*/.*" short-dir)
            (list (file-name-directory short-dir)
                  (if (file-directory-p directory)
                      (file-name-nondirectory short-dir)
                    (file-name-base short-dir)))
          (list "" short-dir)))

      (defvar t-eshell-success-face 'doom-modeline-info)
      (defvar t-eshell-error-face 'doom-modeline-urgent)
      (setq eshell-prompt-function
            (lambda ()
              (let* ((pwd (eshell/pwd))
                     (directory (split-directory-prompt pwd (pwd-shorten-dirs (pwd-replace-home pwd))))
                     (parent (car directory))
                     (name (cadr directory))
                     (branch (or (curr-dir-git-branch-string (eshell/pwd)) ""))
                     (prompt (concat
                              (propertize parent 'face 'font-lock-builtin-face)
                              (propertize name 'face 'font-lock-constant-face)
                              (propertize branch 'face 'font-lock-comment-face)
                              (propertize " $" 'face (if (zerop eshell-last-command-status) t-eshell-success-face t-eshell-error-face))
                              (propertize " " 'face 'font-lock-preprocessor-face))))
                (t/propertize-read-only prompt)))))))

(progn
  ;; eshell git completion

  (defconst pcmpl-git-commands
    '("pr"
      "add" "bisect" "branch" "checkout" "clone"
      "commit" "diff" "fetch" "grep"
      "init" "log" "merge" "mv" "pull" "push" "rebase"
      "reset" "rm" "show" "status" "tag" )
    "List of `git' commands")

  (defun pcmpl-git-remotes ()
    "Return list of `git' remotes."
    (-drop-last 1 (s-split "\r?\n" (shell-command-to-string "git remote show"))))

  (defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
    "The `git' command to run to get a list of refs")

  (defun pcmpl-git-get-refs (types)
    "Return a list of `git' refs filtered by TYPE."
    (with-temp-buffer
      (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
      (goto-char (point-min))
      (let ((ref-list))
        (dolist (type types)
          (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
            (add-to-list 'ref-list (match-string 1))))
        ref-list)))

  (require 'pcomplete)
  (defun pcomplete/git ()
    "Completion for `git'."

    (pcomplete-here* pcmpl-git-commands)
    (cond
     ;; complete files/dirs forever if the command is `add' or `rm'
     ((pcomplete-match (regexp-opt '("add" "rm")) 1)
      (while (pcomplete-here (pcomplete-entries))))
     ((pcomplete-match (regexp-opt '("pr")) 1)
      (while (pcomplete-here (append (pcmpl-git-get-refs '("heads")) (pcmpl-git-remotes)))))
     ;; provide branch completion for the command `checkout'.
     ((pcomplete-match "\\(co\\|checkout\\|merge\\|branch\\|diff\\)" 1)
      (pcomplete-here* (pcmpl-git-get-refs '("heads")))))))

(defun pcomplete/kill ()
  (while (pcomplete-match "^-" 'last) (pcomplete-here '("-1" "-2" "-3" "-6" "-9" "-14" "-15" "-l" "-s")))
  (while (and (pcomplete-match "" 'last)
              (pcomplete-match "-s" 'last -1)) (pcomplete-here '("HUP" "SIGHUP" "SIGINT" "SIGKILL" "SIGTERM" "SIGSTOP")))
  (while (pcomplete-here* (-map 's-trim (-> (shell-command-to-string "ps -eo pid | grep -v PID")
                                            (split-string "\n"))))))

;; pcomplete example
(defun pcomplete/torgeir ()
  (pcomplete-here* '("add" "remove"))
  (cond
   ((pcomplete-match "add" 1) (pcomplete-here* '("one" "two")))
   ((pcomplete-match "remove" 1) (pcomplete-here* '("two" "three")))))


(use-package pcmpl-git
  :commands eshell)

(use-package pcmpl-args
  :commands eshell
  :config
  (defun pcmpl-args-default-man-function (name)
    "torgeir: Patched to remove arguments to work on os x."
    (let ((process-environment process-environment))
      (push "MANWIDTH=10000" process-environment)
      (pcmpl-args-process-file "man" "--" name))))

(use-package pcmpl-homebrew
  :commands eshell)

(use-package pcomplete-extension
  :commands eshell)

;; make ret work on ls results
(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

(provide 't-shell)

