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
   '(better-defaults
     bash-completion
     browse-kill-ring
     change-inner
     cider
     css-eldoc
     dash-at-point
     diminish
     dired-details
     evil
     expand-region
     fill-column-indicator
     find-file-in-project
     flx
     flx-ido
     flycheck
     flycheck-clojure
     flycheck-pos-tip
     gist
     git-gutter
     hackernews
     highlight-escape-sequences
     ido-at-point
     ido-ubiquitous
     jump-char
     magit
     move-text
     multiple-cursors
     paredit
     rainbow-delimiters
     restclient
     s
     shell-command
     simplezen
     smartparens
     smex
     transpose-frame
     undo-tree
     visual-regexp
     whitespace-cleanup-mode
     yasnippet

     arduino-mode
     clojure-mode
     clojure-mode-extra-font-locking
     css-mode
     gitconfig-mode
     gitignore-mode
     js2-mode
     less-css-mode
     markdown-mode
     rainbow-mode)))

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
(require 'setup-hippie)

(require 'setup-langs)
