(defun t/workspace-overlay--buffer-in-current-workspace-p (buffer)
  "Return non-nil when BUFFER belongs to the current workspace."
  (condition-case nil
      (if (and (fboundp 'get-current-persp)
               (fboundp 'persp-buffers))
          (memq buffer (persp-buffers (get-current-persp)))
        t)
    (error t)))

(defun t/workspace-overlay--mode-line-contains-workspace-p (window name)
  "Return non-nil when WINDOW's mode line shows NAME."
  (condition-case nil
      (with-current-buffer (window-buffer window)
        (let ((rendered (format-mode-line mode-line-format nil window)))
          (and (stringp rendered)
               (string-match-p (regexp-quote name) rendered))))
    (error nil)))

(defun t/workspace-overlay--name (window)
  "Return the current workspace name for WINDOW, or nil when unknown."
  (let ((buffer (window-buffer window)))
    (when (t/workspace-overlay--buffer-in-current-workspace-p buffer)
      (let ((name (or (condition-case nil
                          (when (fboundp '+workspace-current-name)
                            (+workspace-current-name))
                        (error nil))
                      (condition-case nil
                          (when (and (fboundp 'persp-name)
                                     (fboundp 'get-current-persp))
                            (persp-name (get-current-persp)))
                        (error nil)))))
        (when (and name
                   (t/workspace-overlay--mode-line-contains-workspace-p
                    window name))
          name)))))

(defun t/workspace-overlay--label (window)
  "Return the overlay label for WINDOW."
  (let* ((buffer (window-buffer window))
         (path (or (buffer-file-name buffer)
                   (buffer-name buffer)))
         (dired (with-current-buffer buffer
                  (derived-mode-p 'dired-mode)))
         (workspace (unless dired
                      (t/workspace-overlay--name window)))
         (label (if workspace
                    (format "%s: %s" workspace path)
                  path)))
    (propertize label
                'face (list 'header-line
                            '(:height 0.8 :foreground "#333")))))

(defun t/workspace-overlay--close-button ()
  "Return the clickable close button."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (with-selected-window (posn-window (event-start event))
                    (kill-current-buffer))))
    (propertize "x"
                'face (list 'header-line
                            '(:height 0.8 :foreground "#333"))
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
                                              (string-pixel-width right))))
                            'face 'header-line)))
    (list align right)))

(defun t/workspace-overlay--apply-window (window &rest _args)
  "Apply header line to WINDOW.

Accept extra ARGS for compatibility with window hooks."
  (let ((target (cond
                 ((windowp window) window)
                 ((framep window) (frame-selected-window window))
                 (t (selected-window)))))
    (when (window-live-p target)
      (set-window-parameter target
                            'header-line-format
                            (when t/workspace-overlay-mode
                              (t/workspace-overlay--format target))))))

(defun t/workspace-overlay--clear ()
  "Clear overlay header line on all windows."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame 'no-minibuf))
      (when (window-live-p window)
        (set-window-parameter window 'header-line-format nil)))))

(defun t/workspace-overlay-refresh (&rest _args)
  "Refresh overlay on all windows."
  (interactive)
  (dolist (frame (frame-list))
    (dolist (window (window-list frame 'no-minibuf))
      (t/workspace-overlay--apply-window window))))

(defun t/workspace-overlay--on-window-buffer-change (window &rest _args)
  "Update overlay for WINDOW after a buffer change."
  (t/workspace-overlay--apply-window window))

(defun t/workspace-overlay--on-window-selection-change (window &rest _args)
  "Update overlay for WINDOW after a selection change."
  (t/workspace-overlay--apply-window window))

(define-minor-mode t/workspace-overlay-mode
  "Show the current workspace in the top-right corner."
  :global t
  (if t/workspace-overlay-mode
      (progn
        (add-hook 'window-selection-change-functions
                  #'t/workspace-overlay--on-window-selection-change)
        (add-hook 'window-buffer-change-functions
                  #'t/workspace-overlay--on-window-buffer-change)
        (add-hook '+workspace-switch-hook
                  #'t/workspace-overlay-refresh)
        (add-hook 'after-load-theme-hook
                  #'t/workspace-overlay-refresh)
        (add-hook 'doom-after-reload-hook
                  #'t/workspace-overlay-refresh)
        (t/workspace-overlay-refresh))
    (remove-hook 'window-selection-change-functions
                 #'t/workspace-overlay--on-window-selection-change)
    (remove-hook 'window-buffer-change-functions
                 #'t/workspace-overlay--on-window-buffer-change)
    (remove-hook '+workspace-switch-hook
                 #'t/workspace-overlay-refresh)
    (remove-hook 'after-load-theme-hook
                 #'t/workspace-overlay-refresh)
    (remove-hook 'doom-after-reload-hook
                 #'t/workspace-overlay-refresh)
    (t/workspace-overlay--clear)))

(t/workspace-overlay-mode 1)
