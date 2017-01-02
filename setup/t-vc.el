(t/declare-prefix "g" "Git")

(use-package git-gutter+
  :diminish git-gutter+-mode
  :defer t
  :init
  (add-hook 'after-init-hook 'global-git-gutter+-mode)
  :config
  (setq git-gutter+-modified-sign "~"
        git-gutter+-added-sign "+"
        git-gutter+-deleted-sign "-"
        git-gutter+-separator-sign (if has-gui "" " "))
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

  (use-package git-gutter-fringe+
    :config
    (fringe-helper-define 'git-gutter-fr+-added '(top repeat) "...XXXX......")
    (fringe-helper-define 'git-gutter-fr+-deleted '(top repeat) "...XXXX......")
    (fringe-helper-define 'git-gutter-fr+-modified '(top repeat) "...XXXX......")
    (git-gutter+-enable-fringe-display-mode)))

(use-package helm-open-github
  :commands (helm-open-github-from-issues
             helm-open-github-from-commit
             helm-open-github-from-file
             helm-open-github-from-pull-requests)
  :init
  (t/declare-prefix "go" "Open github"
                    "i" 'helm-open-github-from-issues
                    "c" 'helm-open-github-from-commit
                    "f" 'helm-open-github-from-file
                    "p" 'helm-open-github-from-pull-requests))

(use-package git-link
  :commands git-link
  :init
  (setq git-link-open-in-browser t)
  (t/declare-prefix "go" "Open github"
                    "l" 'git-link))

(use-package git-timemachine
  :commands git-timemachine-toggle
  :init
  (t/declare-prefix "g" "Git"
                    "T" 'git-timemachine-toggle)
  :config
  (defadvice git-timemachine-mode (after toggle-evil activate)
    (when git-timemachine-mode
      (bind-key "C-n" 'git-timemachine-show-next-revision evil-normal-state-local-map)
      (bind-key "C-p" 'git-timemachine-show-previous-revision evil-normal-state-local-map))))

(use-package gist
  :commands (gist-list
             gist-buffer
             gist-buffer-private
             gist-region
             gist-region-private)
  :init
  (t/declare-prefix "gg" "Gist"
                    "l" 'gist-list
                    "b" 'gist-buffer
                    "B" 'gist-buffer-private
                    "r" 'gist-region
                    "R" 'gist-region-private))

(use-package magit
  :commands magit-status
  :init
  (t/declare-prefix "g" "Git"
                    "s" 'magit-status
                    "b" 'magit-blame
                    "l" 'magit-log
                    "C" 'magit-commit)

  (setq magit-pull-arguments nil
        magit-fetch-arguments '("--prune")
        magit-rebase-arguments '("--interactive")
        magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))

  :config
  (bind-key "q" #'magit-quit-session magit-status-mode-map)

  (add-hook 'magit-log-mode-hook 'visual-line-mode)

  (defadvice magit-blame-mode (after switch-to-emacs-mode activate)
    (if magit-blame-mode
        (evil-emacs-state 1)
      (evil-normal-state 1)))

  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (git-gutter+-refresh)))

(use-package magit-gh-pulls
  :after magit
  :init (add-hook 'magit-mode-hook #'turn-on-magit-gh-pulls))

(provide 't-vc)
