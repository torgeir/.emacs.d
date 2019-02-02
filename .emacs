(defun t/user-file (path)
  (concat (if is-mac "/Users/" "/home/")
          (if is-win "torgth" (replace-regexp-in-string "\\." "" (getenv "USER")))
          "/"
          path))

(setq user-emacs-directory "~/.emacs.d/")

(defun t/user-emacs-file (path)
  (concat user-emacs-directory path))

(setq user-init-file (t/user-emacs-file "init.el"))
(load user-init-file)

(provide '.emacs)
