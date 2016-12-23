;;(package-initialize) stfu
;; credits spacemacs for all the clever stuff
(load (locate-user-emacs-file "t-before.el") t)

(defun t/user-file (path) (expand-file-name (locate-user-emacs-file path)))

(defconst t-leader "SPC")
(defconst t-dir-snippets (t/user-file "snippets"))
(defconst t-dir-setup (t/user-file "setup"))
(defconst t-dir-langs (t/user-file "langs"))
(defconst t-dir-site-lisp (t/user-file "site-lisp"))
(defconst t-file-autoloads (t/user-file "setup/autoloads.el"))
(defconst t-font-size 20)

(defvar *t-debug-init* nil "Debug/time startup")

(load (t/user-file "setup/t-bootstrap.el"))
(require 't-packaging)
(require 't-autoloads)

(t/timing-start)

(dolist (module `(,(when is-mac 't-mac)
                  ,(when is-ms 't-cygwin)
                  t-sane-defaults
                  t-custom
                  t-which-key
                  t-evil
                  t-load-theme
                  t-vc
                  t-keys
                  t-typography
                  t-editor
                  t-shell
                  t-org
                  t-modeline
                  t-langs))
  (when module
    (require module)))

(t/load-theme)

(unless (fboundp 'server-running-p) (require 'server))
(unless (server-running-p) (server-mode))

(t/timing-end)
