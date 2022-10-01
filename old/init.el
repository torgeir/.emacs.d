;;; -*- lexical-binding: t; -*-
;; TODO torgeir remove
(setq comp-deferred-compilation-deny-list '())

;; Default to calling straight-use-package when running use-package.
(setq straight-use-package-by-default t)
(setq straight-check-for-modifications 'live)
(setq gc-cons-threshold most-positive-fixnum  ; no gc while startup
      gc-cons-percentage 0.6)

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

;; TODO torgeir remove
;; https://github.com/hlissner/doom-emacs/issues/4534
(load "~/.emacs.d/fix-wrong-args.el" nil 'nomessage)

;; needs to come before org is loaded for straight.el to choose it instead
(straight-use-package
 '(t-org
   :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
   :local-repo "org"
   :files (:defaults "lisp/*.el")
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
(defvar *t-debug-init* nil "Debug/time startup")
(when *t-debug-init* (setq debug-on-error nil))

(defconst user-emacs-directory "~/.emacs.d/")
(defun t/os-home-prefix () (cond (is-mac "/Users/") (is-linux "/home/") (is-win "c:/Users/")))
(defun t/user-emacs-file (path) (concat user-emacs-directory path))
(defun t/user-file (path) (concat (t/os-home-prefix) (downcase (replace-regexp-in-string "\\." "" (getenv "USER"))) "/" path))
(defun t/user-dropbox-folder (path) (concat (t/user-file (concat "Dropbox" (if is-win  "\(Personlig\)"))) "/" path))

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

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 16 1024 1024) ; bring back gc settings
         gc-cons-percentage 0.1)))
