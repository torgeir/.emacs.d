;;; -*- lexical-binding: t; -*-

;; Default to calling straight-use-package when running use-package.
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications 'live)
(setq gc-cons-threshold (* 1000 1000 1000)) ; 1gb while loading init

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
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/straight/build/use-package/")
  (require 'use-package))

;; needs to come before org is loaded for straight.el to choose it instead
(straight-use-package
 '(org-plus-contrib
   :repo "https://code.orgmode.org/bzg/org-mode.git"
   :local-repo "org"
   :files (:defaults "contrib/lisp/*.el")
   :includes (org)))

(defconst t-leader "SPC")
(defconst t-emacs-leader "C-")
(defconst t-font-size 17)
(defconst lat-trh 63.427)
(defconst lon-trh 10.391)
(defconst loc-trh "Trondheim, Norway")

(defconst is-mac (equal system-type 'darwin))
(defconst is-cygwin (equal system-type 'cygwin))
(defconst is-linux (equal system-type 'gnu/linux))
(defconst is-win (equal system-type 'windows-nt))
(defconst is-ms (or is-cygwin is-win))
(defconst has-gui (display-graphic-p))

(defvar *t-indent* 2)
(defvar *t-indent-xml* 4)
(defvar *t-debug-init* t "Debug/time startup")
(when *t-debug-init* (setq debug-on-error nil))

(defconst user-emacs-directory "~/.emacs.d/")
(defun t/user-emacs-file (path) (concat user-emacs-directory path))
(defun t/user-file (path)
  (concat (if is-mac "/Users/"
            (if is-linux "/home/" "c:/Users/"))
          (if is-win "torgth" (replace-regexp-in-string "\\." "" (getenv "USER")))
          "/"
          path))
(defconst t-user-dropbox-folder (if (or is-mac is-linux)
                                    (t/user-file "Dropbox")
                                  "c:/Users/torgth/Dropbox \(Personlig\)"))
(defun t/user-dropbox-folder (path) (concat t-user-dropbox-folder "/" path))

(defconst t-dir-snippets (t/user-emacs-file "snippets"))
(add-to-list 'load-path (t/user-emacs-file "setup"))
(add-to-list 'load-path t-dir-snippets)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(let ((compile (not (file-exists-p (t/user-emacs-file "readme.elc")))))
  (org-babel-load-file "~/.emacs.d/readme.org" compile)
  (when compile (restart-emacs)))

(setq gc-cons-threshold (* 2 1000 1000))
