(setq *t-spacemacs* (file-exists-p "~/spacemacs"))

(defun t/user-file (path)
  (concat "/Users/" (getenv "USER") "/" path))

(defun t/user-emacs-directory ()
  (if *t-spacemacs* "~/spacemacs/" "~/.emacs.d/"))

(defun t/user-emacs-file (path)
  (concat "~/.emacs.d/" path))

(setq user-emacs-directory (t/user-emacs-directory))
(setq user-init-file (concat (t/user-emacs-directory) "init.el"))
(load user-init-file)

(provide '.emacs)