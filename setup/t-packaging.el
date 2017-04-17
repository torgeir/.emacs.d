(setq load-prefer-newer t) ; don't load outdated bytecode

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)
;;(setq package-check-signature nil) ; when checking signatures fail

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t) ; fetch the ones missing

  (require 'cl)) ; lexical-let
(defvar use-package-verbose *t-debug-init*)
(require 'diminish)
(require 'bind-key)

(require 't-local)

(t/use-package paradox
  :commands (paradox-list-packages paradox-upgrade-packages)
  :evil-state ((paradox-menu-mode . emacs)
               (paradox-commit-list-mode . emacs))
  :init
  (setq paradox-github-token t
        paradox-execute-asynchronously t))

(provide 't-packaging)
