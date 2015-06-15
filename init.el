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
     guide-key
     hackernews
     highlight-escape-sequences
     ido-at-point
     ido-ubiquitous
     jump-char
     magit
     move-text
     multiple-cursors
     paredit
     powerline
     rainbow-delimiters
     restclient
     s
     shell-command
     simplezen
     smartparens
     smart-mode-line
     smex
     tagedit
     transpose-frame
     undo-tree
     visual-regexp
     whitespace-cleanup-mode
     w3m
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

(require 'appearance)

(load-theme 'gruvbox t)

(require 'util)

(when is-mac
  (require 'mac))

(require 'editor)
(require 'keys)

(setq magit-last-seen-setup-instructions "1.4.0")
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'shell '(require 'setup-shell))
(eval-after-load 'yasnippet '(require 'setup-yasnippet))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-guide-key)
(require 'setup-evil)
(require 'setup-paredit)
(require 'setup-smartparens)
(require 'setup-visual-regexp)
(require 'setup-gist)
(require 'setup-dash)
(require 'setup-hippie)
(require 'setup-langs)
