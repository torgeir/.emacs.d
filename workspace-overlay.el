(defun t/workspace-overlay--name ()
  "Return the current workspace name."
  (or (condition-case nil
          (when (fboundp '+workspace-current-name)
            (+workspace-current-name))
        (error nil))
      (condition-case nil
          (when (and (fboundp 'persp-name)
                     (fboundp 'get-current-persp))
            (persp-name (get-current-persp)))
        (error nil))
      "default"))

(defun t/workspace-overlay--label (window)
  "Return the overlay label for WINDOW."
  (let* ((buffer (window-buffer window))
         (path (or (buffer-file-name buffer)
                   (buffer-name buffer))))
    (propertize (format "%s: %s" (t/workspace-overlay--name) path)
                'face '(:height 0.8))))

(defun t/workspace-overlay--close-button ()
  "Return the clickable close button."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (with-selected-window (posn-window (event-start event))
                    (kill-current-buffer))))
    (propertize "x"
                'face '(:height 0.8)
                'mouse-face 'mode-line-highlight
                'help-echo "Close buffer"
                'local-map map)))

(defun t/workspace-overlay--format (window)
  "Return the header line for WINDOW."
  (let* ((label (t/workspace-overlay--label window))
         (close (t/workspace-overlay--close-button))
         (right (concat " " close " " label))
         (align (propertize " "
                            'display
                            (list (list 'space :align-to
                                        (list '- 'right-edge
                                              (string-pixel-width right)))))))
    (list align right)))

(defun t/workspace-overlay--apply-window (window)
  "Apply header line to WINDOW."
  (when (window-live-p window)
    (set-window-parameter window
                          'header-line-format
                          (t/workspace-overlay--format window))))

(defun t/workspace-overlay-refresh ()
  "Refresh overlay on all windows."
  (interactive)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame 'no-minibuf))
      (t/workspace-overlay--apply-window window))))

(define-minor-mode t/workspace-overlay-mode
  "Show the current workspace in the top-right corner."
  :global t
  (if t/workspace-overlay-mode
      (progn
        (add-hook 'window-selection-change-functions
                  #'t/workspace-overlay-refresh)
        (add-hook 'window-buffer-change-functions
                  #'t/workspace-overlay-refresh)
        (add-hook '+workspace-switch-hook
                  #'t/workspace-overlay-refresh)
        (add-hook 'after-load-theme-hook
                  #'t/workspace-overlay-refresh)
        (add-hook 'doom-after-reload-hook
                  #'t/workspace-overlay-refresh)
        (t/workspace-overlay-refresh))
    (remove-hook 'window-selection-change-functions
                 #'t/workspace-overlay-refresh)
    (remove-hook 'window-buffer-change-functions
                 #'t/workspace-overlay-refresh)
    (remove-hook '+workspace-switch-hook
                 #'t/workspace-overlay-refresh)
    (remove-hook 'after-load-theme-hook
                 #'t/workspace-overlay-refresh)
    (remove-hook 'doom-after-reload-hook
                 #'t/workspace-overlay-refresh)))

(t/workspace-overlay-mode 1)
