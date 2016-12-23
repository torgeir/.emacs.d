(defconst is-mac (equal system-type 'darwin))
(defconst is-cygwin (equal system-type 'cygwin))
(defconst is-linux (equal system-type 'gnu/linux))
(defconst is-win (equal system-type 'windows-nt))
(defconst is-ms (or is-cygwin is-win))
(defconst has-gui (display-graphic-p))

(add-to-list 'load-path t-dir-setup)
(add-to-list 'load-path t-dir-langs)
(add-to-list 'load-path t-dir-site-lisp)

(dolist (project (directory-files t-dir-site-lisp t "\\w+")) ; add folders inside site-lisp as well
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

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

(provide 't-bootstrap)
