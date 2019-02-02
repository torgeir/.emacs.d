;;; -*- lexical-binding: t; -*-
(package-initialize) ; stfu

(load (t/user-emacs-file "t-before.el") t)
(load (t/user-emacs-file "t-bootstrap.el"))

(t/timing-start)

(require 't-macros)
(require 't-packaging)

(when is-mac    (require 't-mac)    (t-mac/init))
(when is-linux  (require 't-linux)  (t-linux/init))
(when is-cygwin (require 't-cygwin) (t-cygwin/init))

(require 't-sane-defaults)
(t-sane-defaults/init)

(setq custom-file (t/user-emacs-file "custom.el"))
(load custom-file)

(require 't-site-lisp)
(require 't-calendar)
(require 't-which-key)
(require 't-load-theme)
(require 't-evil)
(require 't-vc)
(require 't-keys)
(require 't-editor)
(require 't-shell)
(require 't-org)
(require 't-text-objects)
(require 't-typography)
(require 't-modeline)

(require 't-lang-arduino)
(require 't-lang-applescript)
(require 't-lang-clojure)
(require 't-lang-css)
(require 't-lang-docker)
(require 't-lang-elisp)
(require 't-lang-elm)
(require 't-lang-flycheck)
(require 't-lang-gitconfig)
(require 't-lang-gitignore)
(require 't-lang-haskell)
(require 't-lang-js)
(require 't-lang-json)
(require 't-lang-markdown)
(require 't-lang-pug)
(require 't-lang-php)
(require 't-lang-remark)
(require 't-lang-scala)
(require 't-lang-sh)
(require 't-lang-html)
(require 't-lang-yaml)
(require 't-lang-terraform)

(dolist (pkg t-use-package-pkgs) (funcall pkg))

(when is-mac    (t-mac/config))
(when is-linux  (t-linux/config))
(when is-cygwin (t-cygwin/config))

(t-load-theme/config)
(t-evil/config)
(t-vc/config)
(t-keys/config)
(t-typography/config)
(t-editor/config)
(t-shell/config)
(t-org/config)
(t-modeline/config)

(unless (fboundp 'server-running-p) (require 'server))
(unless (server-running-p) (server-mode))

(t/timing-end)
