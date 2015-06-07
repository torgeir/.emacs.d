(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))
(setq vendor-dir
      (expand-file-name "vendor" user-emacs-directory))
(setq themes-dir
      (expand-file-name "themes" user-emacs-directory))

(add-to-list 'load-path settings-dir)
(add-to-list 'load-path vendor-dir)
(add-to-list 'custom-theme-load-path themes-dir)

(package-initialize)

(require 'appearance)
(require 'setup-package)

(defun install-packages ()
  (packages-install
    '(
      better-defaults
      browse-kill-ring
      bash-completion
      paredit
      clojure-mode
      clojure-mode-extra-font-locking
      cider
      visual-regexp
      css-eldoc
      diminish
      transpose-frame
      flx
      flx-ido
      find-file-in-project
      ido-at-point
      ido-ubiquitous
      dired-details
      fill-column-indicator
      flycheck
      flycheck-clojure
      flycheck-pos-tip
      gist
      magit
      jump-char
      shell-command
      smartparens
      evil
      smex
      s
      move-text
      expand-region
      undo-tree
      multiple-cursors
      rainbow-delimiters
      rainbow-mode
      css-mode
      gitignore-mode
      gitconfig-mode
      git-gutter
      highlight-escape-sequences
      whitespace-cleanup-mode
      dash-at-point
      restclient
      hackernews
      yasnippet
      simplezen
      arduino-mode
      markdown-mode
      js2-mode)))

(condition-case nil
    (install-packages)
  (error
   (package-refresh-contents)
   (install-packages)))

(load-theme 'gruvbox t)

(require 'util)

(when is-mac
  (require 'mac))

(require 'editor)
(require 'keys)

(eval-after-load 'ido
  '(require 'setup-ido))

(require 'setup-evil)
(require 'setup-shell)
(require 'setup-paredit)
(require 'setup-smartparens)
(require 'setup-visual-regexp)
(require 'setup-magit)
(require 'setup-gist)
(require 'setup-yasnippet)
(require 'setup-dash)
(require 'setup-dired)

(require 'setup-langs)
