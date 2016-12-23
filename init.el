; credits spacemacs for all the clever stuff
(load (locate-user-emacs-file "t-before.el") t)

(setq is-mac (equal system-type 'darwin)
      is-cygwin (equal system-type 'cygwin)
      is-linux (equal system-type 'gnu/linux)
      is-win (equal system-type 'windows-nt)
      is-ms (or is-cygwin is-win)
      has-gui (display-graphic-p))

(defvar *user-leader* "SPC")
(defvar *user-dir-snippets* (locate-user-emacs-file "snippets"))
(defvar *user-dir-setup* (locate-user-emacs-file "setup"))
(defvar *user-dir-langs* (locate-user-emacs-file "langs"))
(defvar *user-dir-site-lisp* (locate-user-emacs-file "site-lisp"))
(defvar t-font-size 14)

(add-to-list 'load-path *user-dir-setup*)
(add-to-list 'load-path *user-dir-langs*)
(add-to-list 'load-path *user-dir-site-lisp*)
(dolist (project (directory-files *user-dir-site-lisp* t "\\w+")) ; add folders inside site-lisp as well
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;(package-initialize)
(require 't-packaging)

(defvar *t-debug-init* nil "Debug/time startup")
(when *t-debug-init*
  (message "t: timing init")
  (require 't-debug)

  ;; benchmarks
  (use-package benchmark-init :config (benchmark-init/activate)))

(dolist (module `(t-defuns
                  ,(when is-mac 't-mac)
                  ,(when is-ms 't-cygwin)
                  t-sane-defaults
                  t-which-key
                  t-evil
                  t-typography
                  t-shell
                  t-vc
                  t-org
                  t-keys
                  t-load-theme
                  t-editor
                  t-modeline
                  t-langs))
  (when module
    (require module)))

(unless (fboundp 'server-running-p) (require 'server))
(unless (server-running-p) (server-mode))

;; benchmarks
(when *t-debug-init*
  (message "t: timing init complete")
  (benchmark-init/show-durations-tabulated)
  (benchmark-init/show-durations-tree))

(evil-mode 1)
(t/load-theme)
