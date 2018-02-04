;;; -*- lexical-binding: t; -*-
(t/use-package git-gutter+
  :diminish git-gutter+-mode
  :init
  (t/add-hook 'prog-mode-hook 'git-gutter+-mode)
  :config
  (progn
    (setq git-gutter+-modified-sign "~"
          git-gutter+-added-sign "+"
          git-gutter+-deleted-sign "-"
          git-gutter+-separator-sign (if has-gui "" " "))))

(t/use-package git-gutter-fringe+
  :init
  (t/add-hook-defun 'git-gutter+-mode-hook t/hook-git-gutter+
                    (fringe-helper-define 'git-gutter-fr+-added '(top repeat) "XXX.....")
                    (fringe-helper-define 'git-gutter-fr+-delprivate/t/eted '(top repeat) "XXX.....")
                    (fringe-helper-define 'git-gutter-fr+-modified '(top repeat) "XXX.....")
                    (git-gutter+-enable-fringe-display-mode)))

(t/use-package helm-open-github
  :commands (helm-open-github-from-issues
             helm-open-github-from-commit
             helm-open-github-from-file
             helm-open-github-from-pull-requests))

(t/use-package git-link
  :commands git-link
  :init
  (setq git-link-open-in-browser t))

(t/use-package git-timemachine
  :commands git-timemachine-toggle
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    (when git-timemachine-mode
      (t/bind-in 'evil-normal-state-local-map
                 "C-n" 'git-timemachine-show-next-revision
                 "C-p" 'git-timemachine-show-previous-revision))))

(t/use-package gist
  :commands (gist-list
             gist-buffer
             gist-buffer-private
             gist-region
             gist-region-private))

(t/use-package magit
  :commands magit-status
  :init
  (setq magit-pull-arguments nil
        magit-fetch-arguments '("--prune")
        magit-rebase-arguments '("--interactive")
        magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))

  :config
  (progn
    (bind-key "q" #'magit-quit-session magit-status-mode-map)

    (t/add-hook 'magit-log-mode-hook 'visual-line-mode)

    (when (boundp 'spacemacs-useful-buffers-regexp)
      (add-to-list 'spacemacs-useful-buffers-regexp "\\*magit.*"))

    (defadvice magit-blame-mode (after switch-to-emacs-mode activate)
      (if magit-blame-mode
          (evil-emacs-state 1)
        (evil-normal-state 1)))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (git-gutter+-refresh))))

(t/use-package magit-gh-pulls
  :after magit
  :init (t/add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(defun t-vc/config ()
  (t/declare-prefix "g" "Git"
                    "T" 'git-timemachine-toggle
                    "s" 'magit-status
                    "b" 'magit-blame
                    "l" 'magit-log
                    "C" 'magit-commit
                    "c" #'t/clone)

  (t/declare-prefix "gh" "Hunk"
                    "n" 'git-gutter+-next-hunk
                    "N" 'git-gutter+-previous-hunk
                    "C" 'git-gutter+-stage-and-commit
                    "?" 'git-gutter+-show-hunk-inline-at-point
                    "=" 'git-gutter+-show-hunk
                    "r" 'git-gutter+-revert-hunks
                    "s" 'git-gutter+-stage-hunks
                    "cc" 'magit-commit
                    "ca" 'magit-commit-amend)

  (t/declare-prefix "go" "Open github"
                    "l" 'git-link
                    "i" 'helm-open-github-from-issues
                    "c" 'helm-open-github-from-commit
                    "f" 'helm-open-github-from-file
                    "p" 'helm-open-github-from-pull-requests)

  (t/declare-prefix "gg" "Gist"
                    "l" 'gist-list
                    "b" 'gist-buffer
                    "B" 'gist-buffer-private
                    "r" 'gist-region
                    "R" 'gist-region-private))

(provide 't-vc)
