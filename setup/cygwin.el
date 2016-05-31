(provide 'cygwin)

(global-set-key (kbd "C-+") 't/increase-font-size)
(global-set-key (kbd "C--") 't/decrease-font-size)
(global-set-key (kbd "C-0") 't/reset-font-size)

(defun make-auto-save-file-name ()
  "torgeir: copied this from ftp://ftp.gnu.org/old-gnu/emacs/windows/docs/faq8.html. Fixes an issue when in gui emacs on windows it cant save backup files.
  
  Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  This version stores all auto-save files in the
same local directory. This is to avoid trying to save files over a dial-up
connection (which may not be active).  See also `auto-save-file-name-p'."
  (if buffer-file-name
      (if (and (eq system-type 'ms-dos)
	       (not (msdos-long-file-names)))
	  (let ((fn (file-name-nondirectory buffer-file-name)))
		(string-match "\\`\\([^.]+\\)\\(\\.\\(..?\\)?.?\\|\\)\\'" fn)
	    (concat (expand-file-name "~/save/")
		    "#" (match-string 1 fn) 
		    "." (match-string 3 fn) "#"))
	(concat (expand-file-name "~/.save/")
		"#"
		(file-name-nondirectory buffer-file-name)
		"#"
		(make-temp-name "")))

    ;; Deal with buffers that don't have any associated files.  (Mail
    ;; mode tends to create a good number of these.)

    (let ((buf-name (buffer-name))
	  (limit 0))

      ;; Use technique from Sebastian Kremer's auto-save
      ;; package to turn slashes into \\!.  This ensures that
      ;; the auto-save buffer name is unique.

      (while (string-match "[/\\*?':]" buf-name limit)
	(message "%s" buf-name)
	(setq buf-name (concat (substring buf-name 0 (match-beginning 0))
			(if (string= (substring buf-name
						(match-beginning 0)
						(match-end 0))
				     "/")
			    "\\!"
			  (if (string= (substring buf-name
						  (match-beginning 0)
						  (match-end 0))
				       "\\\\")
			      "\\\\" "__"))
			(substring buf-name (match-end 0))))
	(setq limit (1+ (match-end 0))))

      (expand-file-name
       (format "~/.save/#%s#%s#" buf-name (make-temp-name ""))))))