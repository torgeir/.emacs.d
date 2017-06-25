(eval-when-compile
  (require 'cl nil t))

(defun t/js2-get-json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (js2-reparse)
  (let (previous-node current-node
                      key-name
                      rlt)

    ;; The `js2-node-at-point' starts scanning from AST root node.
    ;; So there is no way to optimize it.
    (setq current-node (js2-node-at-point))

    (while (not (js2-ast-root-p current-node))
      (cond
       ;; JSON property node
       ((js2-object-prop-node-p current-node)
        (setq key-name (js2-prop-node-name (js2-object-prop-node-left current-node)))
        (if rlt (setq rlt (concat "." key-name rlt))
          (setq rlt (concat "." key-name))))

       ;; Array node
       ((or (js2-array-node-p current-node))
        (setq rlt (concat (js2-get-element-index-from-array-node previous-node
                                                                 current-node
                                                                 hardcoded-array-index)
                          rlt)))

       ;; Other nodes are ignored
       (t))

      ;; current node is archived
      (setq previous-node current-node)
      ;; Get parent node and continue the loop
      (setq current-node (js2-node-parent current-node)))

    (cond
     (rlt
      ;; Clean the final result
      (setq rlt (replace-regexp-in-string "^\\." "" rlt)))
     (t
      (message "No JSON path found!")))

    rlt))

(defun t/json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (cond
   ((memq major-mode '(js2-mode))
    (js2-print-json-path hardcoded-array-index))
   (t
    (let* ((cur-pos (point))
           (str (buffer-substring-no-properties (point-min) (point-max))))
      (when (string= "json" (file-name-extension buffer-file-name))
        (setq str (format "var a=%s;" str))
        (setq cur-pos (+ cur-pos (length "var a="))))
      (unless (featurep 'js2-mode)
        (require 'js2-mode))
      (with-temp-buffer
        (insert str)
        (js2-init-scanner)
        (js2-do-parse)
        (goto-char cur-pos)
        (t/js2-get-json-path))))))

;;;###autoload
(defun json-path-eldoc-function()
  (ignore-errors
    (save-restriction
      (t/json-path))))

(defun turn-on-json-path-eldoc ()
  (set (make-local-variable 'eldoc-documentation-function) 'json-path-eldoc-function)
  (eldoc-mode))

;;;###autoload
(defun json-path-eldoc-enable ()
  "Turn on json-path-eldoc in buffers where `json-mode' is active."
  (interactive)
  (add-hook 'json-mode-hook #'turn-on-json-path-eldoc))

;;;###autoload
(defun json-path-eldoc-disable ()
  "Disable json-path-eldoc."
  (interactive)
  (remove-hook 'json-mode-hook #'turn-on-json-path-eldoc))

(provide 'json-path-eldoc)
;;; json-path-eldoc.el ends here
