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
     etags-select
     evil
     expand-region
     fill-column-indicator
     flx
     flx-ido
     flycheck
     flycheck-clojure
     flycheck-pos-tip
     gist
     git-gutter+
     gruvbox-theme
     ;; guide-key
     which-key
     guru-mode
     hackernews
     highlight-escape-sequences
     ;; ido-at-point ; the latest version from melpa is not working any longer?
     ido-ubiquitous
     ido-vertical-mode
     jump-char
     magit
     move-text
     multiple-cursors
     neotree
     paredit
     powerline
     projectile
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

     company
     company-quickhelp
     company-restclient
     company-tern
     company-web

     arduino-mode
     clojure-mode
     clojure-mode-extra-font-locking
     css-mode
     gitconfig-mode
     gitignore-mode
     json-mode
     js2-mode
     less-css-mode
     markdown-mode
     rainbow-mode
     web-mode)))

(condition-case nil
    (install-packages)
  (error
   (package-refresh-contents)
   (install-packages)))

(require 'appearance)

(load-theme 'gruvbox t)

(require 'util)

(when is-mac (require 'mac))
(when is-cygwin (require 'cygwin))

(require 'editor)
(require 'keys)

(setq magit-last-seen-setup-instructions "1.4.0")
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'shell '(require 'setup-shell))
(eval-after-load 'yasnippet '(require 'setup-yasnippet))
(eval-after-load 'dired '(require 'setup-dired))
(require 'setup-which-key)
;;(require 'setup-evil)
(require 'setup-paredit)
(require 'setup-smartparens)
(require 'setup-visual-regexp)
(require 'setup-gist)
(require 'setup-dash)
(require 'setup-hippie)
(require 'setup-langs)
(require 'setup-powerline)
(require 'setup-org)
(require 'setup-gitgutter)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
