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

(provide 't-autoloads)
