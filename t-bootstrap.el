;;; -*- lexical-binding: t; -*-
(use-package dash) ; list helpers
(use-package s) ; string helpers
(use-package f) ; file helpers

(defconst t-leader "SPC")
(defconst t-emacs-leader "C-")
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
(when *t-debug-init* (setq debug-on-error nil))

(defconst t-dir-setup (t/user-emacs-file "setup"))
(defconst t-dir-snippets (t/user-emacs-file "snippets"))
(defconst t-file-autoloads (t/user-emacs-file "setup/autoloads.el"))

(add-to-list 'load-path t-dir-setup)
(add-to-list 'load-path (t/user-emacs-file "setup/langs"))
(let ((dir-site-lisp (t/user-emacs-file "site-lisp")))
  (add-to-list 'load-path dir-site-lisp)
  ;; add folders inside site-lisp as well
  (dolist (project (directory-files dir-site-lisp t "\\w+"))
    (when (file-directory-p project)
      (add-to-list 'load-path project))))

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
  (when (and (boundp 'generated-autoload-file)
             generated-autoload-file
             (file-exists-p generated-autoload-file))
    (delete-file generated-autoload-file))
  (shell-command (concat "touch " t-file-autoloads))
  (let ((generated-autoload-file t-file-autoloads))
    (update-directory-autoloads (concat t-dir-setup "/"))
    (when (called-interactively-p 'interactive) (load t-file-autoloads t t))))

(unless (require 'autoloads t-file-autoloads t)
  (t/reload-autoloads)
  (unless (require 'autoloads t-file-autoloads t) (error "autoloads.el not generated!")))

(provide 't-bootstrap)
