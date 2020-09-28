(if nil
    (load "~/.emacs.d/init-old.el")
  (progn

    ;; Default to calling straigth-use-package when running use-package.
    (setq straight-use-package-by-default t)

    ;; Bootstrap [straight.el](https://github.com/raxod502/straight.el).
    (defvar bootstrap-version)
    (let ((bootstrap-file
           (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
          (bootstrap-version 5))
      (unless (file-exists-p bootstrap-file)
        (with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
          (goto-char (point-max))
          (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

    ;; Install [use-package](https://github.com/jwiegley/use-package).
    (straight-use-package 'use-package)

    ;; needs to come before org is loaded for straight.el to choose it instead
    (straight-use-package
     '(org-plus-contrib
       :repo "https://code.orgmode.org/bzg/org-mode.git"
       :local-repo "org"
       :files (:defaults "contrib/lisp/*.el")
       :includes (org)))
    (org-babel-load-file "~/.emacs.d/readme.org")))

