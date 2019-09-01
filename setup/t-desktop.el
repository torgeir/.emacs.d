(defun t/desktop-project-name ()
  (when-let ((root (t/project-root)))
    (concat root
            ".desktop-"
            (car (-drop 1 (reverse (split-string root "/")))))))


(defun t/desktop-save ()
  (interactive)
  (when-let ((desktop-base-file-name (t/desktop-project-name)))
    (desktop-save (or desktop-dirname
                      (t/project-root)) t)))


(defun t/desktop-restore ()
  (interactive)
  (if (file-exists-p (expand-file-name (t/desktop-project-name)))
      (when-let ((desktop-base-file-name (t/desktop-project-name))
                 (desktop-dirname (expand-file-name user-emacs-directory)))
        (desktop-revert))
    (progn
      (dired ".")
      (t/desktop-save))))


(provide 't-desktop)