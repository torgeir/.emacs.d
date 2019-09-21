(defun t/desktop-project-name ()
  (when-let ((root (t/project-root)))
    (concat root
            ".desktop-"
            (car (-drop 1 (reverse (split-string root "/")))))))


(defun t/desktop-save ()
  (interactive)
  (if-let ((desktop-base-file-name (t/desktop-project-name)))
      (desktop-save (or desktop-dirname
                        (t/project-root)) t)
    (message "Not in a project.")))


(defun t/desktop-restore ()
  (interactive)
  (if-let (project-name (t/desktop-project-name))
      (if (file-exists-p (expand-file-name project-name))
          (when-let ((desktop-base-file-name project-name)
                     (desktop-dirname (expand-file-name user-emacs-directory)))
            (desktop-revert))
        (progn
          (dired ".")
          (t/desktop-save)))
    (message "Not in a project.")))


(provide 't-desktop)