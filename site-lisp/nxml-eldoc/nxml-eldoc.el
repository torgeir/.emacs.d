(eval-when-compile
  (require 'cl nil t))

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

;;;###autoload
(defun nxml-eldoc-function()
  (ignore-errors
    (save-restriction
      (nxml-where))))

(defun turn-on-nxml-eldoc ()
  (set (make-local-variable 'eldoc-documentation-function) 'nxml-eldoc-function)
  (eldoc-mode))

;;;###autoload
(defun nxml-eldoc-enable ()
  "Turn on nxml-eldoc in buffers where `nxml-mode' is active."
  (interactive)
  (add-hook 'nxml-mode-hook #'turn-on-nxml-eldoc))

;;;###autoload
(defun nxml-eldoc-disable ()
  "Disable nxml-eldoc."
  (interactive)
  (remove-hook 'nxml-mode-hook #'turn-on-nxml-eldoc))

(provide 'nxml-eldoc)
;;; nxml-eldoc.el ends here
