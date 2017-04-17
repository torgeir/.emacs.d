(require 'cl)
(load (t/user-emacs-file "t-before.el") t)

(defconst t-leader "SPC")
(defconst t-dir-snippets (t/user-emacs-file "snippets"))
(defconst t-dir-setup (t/user-emacs-file "setup"))
(defconst t-dir-langs (t/user-emacs-file "langs"))

(defconst t-dir-site-lisp (t/user-emacs-file "site-lisp"))
(defconst t-file-autoloads (t/user-emacs-file "setup/autoloads.el"))
(defconst t-font-size 17)

(defconst is-mac (equal system-type 'darwin))
(defconst is-cygwin (equal system-type 'cygwin))
(defconst is-linux (equal system-type 'gnu/linux))
(defconst is-win (equal system-type 'windows-nt))
(defconst is-ms (or is-cygwin is-win))
(defconst has-gui (display-graphic-p))

(defvar *t-indent* 2)
(defvar *t-indent-xml* 4)
(defvar *t-debug-init* nil "Debug/time startup")

(add-to-list 'load-path t-dir-setup)
(add-to-list 'load-path t-dir-langs)
(add-to-list 'load-path t-dir-site-lisp)

(dolist (project (directory-files t-dir-site-lisp t "\\w+")) ; add folders inside site-lisp as well
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(require 't-macros)

(defun t/timing-start ()
  (interactive)
  (when *t-debug-init*
    (message "t: timing init")
    (require 't-debug)

    ;; benchmarks
    (use-package benchmark-init :config (benchmark-init/activate))))

(defun t/timing-end ()
  (interactive)
  (when *t-debug-init*
    (message "t: timing init complete")
    (benchmark-init/show-durations-tabulated)
    (benchmark-init/show-durations-tree)))

(defun t/reload-autoloads ()
  "Regenerate and reload autoloads.el."
  (interactive)
  (let ((generated-autoload-file t-file-autoloads))
    (when (file-exists-p generated-autoload-file) (delete-file generated-autoload-file))
    (update-directory-autoloads t-dir-setup)
    (when (called-interactively-p 'interactive) (load "autoloads"))))

(unless (require 'autoloads t-file-autoloads t)
  (t/reload-autoloads)
  (unless (require 'autoloads t-file-autoloads t) (error "autoloads.el not generated!")))

(provide 't-bootstrap)
