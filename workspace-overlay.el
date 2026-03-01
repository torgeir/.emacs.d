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

(defun t/workspace-overlay--window-from-args (args)
  "Return a usable window from ARGS."
  (let ((win nil)
        (frame nil))
    (dolist (arg args)
      (cond
       ((windowp arg) (setq win arg))
       ((framep arg) (setq frame arg))))
    (cond
     (win win)
     (frame (frame-selected-window frame))
     (t (selected-window)))))

(defun t/workspace-overlay--buffer-in-current-p (buffer)
  "Return non-nil if BUFFER belongs to the current workspace."
  (when (and (fboundp 'get-current-persp)
             (fboundp 'persp-buffers))
    (condition-case nil
        (memq buffer (persp-buffers (get-current-persp)))
      (error nil))))

(defun t/workspace-overlay--modeline-shows-workspace-p (buffer &optional window)
  "Return non-nil if doom-modeline shows a workspace for BUFFER."
  (when (and (fboundp 'doom-modeline-segment--persp-name)
             (window-live-p window))
    (with-selected-window window
      (with-current-buffer buffer
        (condition-case nil
            (let* ((seg (doom-modeline-segment--persp-name))
                   (text (when (stringp seg) (string-trim seg)))
                   (face (get-text-property 0 'face seg)))
              (and text
                   (not (string-empty-p text))
                   (not (eq face 'doom-modeline-persp-buffer-not-in-persp))))
          (error nil))))))

(defun t/workspace-overlay--buffer-workspace (buffer &optional window)
  "Return the workspace name for BUFFER."
  (let ((fallback (t/workspace-overlay--name)))
    (cond
     ((fboundp 'persp-buffer-in-other-p)
      (with-current-buffer buffer
        (cond
         ((t/workspace-overlay--buffer-in-current-p buffer)
          fallback)
         ((condition-case nil
              (persp-buffer-in-other-p buffer)
            (error nil))
          nil)
         (t fallback))))
     (t fallback))))

(defun t/workspace-overlay--label (&optional window)
  "Return the propertized label for the overlay in WINDOW."
  (let* ((window (or window (selected-window)))
         (buffer (window-buffer window))
         (workspace (t/workspace-overlay--buffer-workspace buffer window))
         (path (or (buffer-file-name buffer)
                   (buffer-name buffer)))
         (label (if workspace
                    (format "%s: %s" workspace path)
                  (format "%s" path))))
    (propertize label
                't/workspace-overlay t
                'face (t/workspace-overlay--face))))

(defun t/workspace-overlay--close-button ()
  "Return the propertized close button for the overlay."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (event)
                  (interactive "e")
                  (with-selected-window (posn-window (event-start event))
                    (kill-current-buffer))))
    (propertize "x"
                't/workspace-overlay t
                'face (t/workspace-overlay--face)
                'mouse-face 'mode-line-highlight
                'help-echo "Close buffer"
                'local-map map)))

(defun t/workspace-overlay--label-px (window label)
  "Return pixel width for LABEL in WINDOW's frame."
  (if (and (display-graphic-p (window-frame window))
           (fboundp 'string-pixel-width))
      (string-pixel-width label)
    (* (frame-char-width (window-frame window))
       (string-width label))))

(defun t/workspace-overlay--align (window label)
  "Return padding to align LABEL at WINDOW's right edge."
  (setq window (or window (selected-window)))
  (if (display-graphic-p (window-frame window))
      (propertize " "
                  'display
                  (list (list 'space :align-to
                              (list '- 'right-edge
                                    (t/workspace-overlay--label-px
                                     window label)))))
    (let* ((width (window-body-width window))
           (pad (max 0 (- width (string-width label)))))
      (make-string pad ?\s))))

(defun t/workspace-overlay--format-p (value)
  "Return non-nil if VALUE is this overlay's header-line format."
  (cond
   ((stringp value)
    (text-property-any 0 (length value)
                       't/workspace-overlay t value))
   ((listp value)
    (seq-some (lambda (item)
                (and (stringp item)
                     (text-property-any 0 (length item)
                                        't/workspace-overlay t item)))
              value))
   (t nil)))

(defun t/workspace-overlay--blank ()
  "Return a blank header-line placeholder for inactive windows."
  (propertize " "
              't/workspace-overlay t
              'face (t/workspace-overlay--blank-face)))

(defun t/workspace-overlay--default-bg ()
  "Return the default background color for the selected frame."
  (or (face-background 'default nil t)
      (frame-parameter nil 'background-color)))

(defun t/workspace-overlay--face ()
  "Return the face for overlay text."
  (let* ((family (face-attribute 'header-line :family nil t))
         (base `(:inherit header-line
                 :slant normal
                 :weight normal
                 :underline nil
                 :box nil
                 :foreground "#666666"
                 :height 0.8)))
    (if family
        (append base (list :family family))
      base)))

(defun t/workspace-overlay--blank-face ()
  "Return the face for blank overlay areas."
  (let* ((family (face-attribute 'header-line :family nil t))
         (base `(:inherit header-line
                 :box nil
                 :height 0.8)))
    (if family
        (append base (list :family family))
      base)))

(defun t/workspace-overlay--format (window active)
  "Return header line showing the current workspace at the right edge."
  (let* ((label (when active (t/workspace-overlay--label window)))
         (close (when active (t/workspace-overlay--close-button)))
         (right (when active
                  (let* ((label-fit
                          (if (display-graphic-p (window-frame window))
                              label
                            (let* ((width (window-body-width window))
                                   (close-width (string-width close))
                                   (available (max 0 (- width close-width 1)))
                                   (fit (truncate-string-to-width label available
                                                                  nil nil "...")))
                              fit)))
                         (close-with-space (concat " " close " ")))
                    (list close-with-space label-fit))))
         (left (let ((prev (window-parameter window
                                             't/workspace-overlay-prev-header-line)))
                 (when (and prev (not (t/workspace-overlay--format-p prev)))
                   (format-mode-line prev nil window)))))
    (if active
        (append (when left (list left)) right)
      (if left
          (concat left (t/workspace-overlay--blank))
        (t/workspace-overlay--blank)))))

(defun t/workspace-overlay--clear-buffer-local ()
  "Clear stale buffer-local header-line overlays from prior versions."
  (when (t/workspace-overlay--format-p header-line-format)
    (setq header-line-format nil)))

(defun t/workspace-overlay--apply-window (window &optional active)
  "Apply workspace header line to WINDOW only."
  (when (window-live-p window)
    (let ((current (window-parameter window 'header-line-format))
          (buffer (window-buffer window)))
      (when (and (null (window-parameter window
                                         't/workspace-overlay-prev-header-line))
                 (not (t/workspace-overlay--format-p current)))
        (set-window-parameter window
                              't/workspace-overlay-prev-header-line
                              current))
      (set-window-parameter window
                            'header-line-format
                            (t/workspace-overlay--format window active)))))

(defun t/workspace-overlay--clear-window (window)
  "Restore WINDOW's header line and clear overlay state."
  (let ((prev (window-parameter window
                                't/workspace-overlay-prev-header-line)))
    (set-window-parameter window 'header-line-format prev)
    (set-window-parameter window 't/workspace-overlay-prev-header-line nil)))

(defun t/workspace-overlay--select-window (&rest args)
  "Show overlay in WINDOW for its frame."
  (let* ((window (t/workspace-overlay--window-from-args args))
         (frame (window-frame window)))
    (when (window-live-p window)
      (set-frame-parameter frame 't/workspace-overlay-window window)
      (dolist (win (window-list frame 'no-minibuf))
        (t/workspace-overlay--apply-window win (eq win window))))))

(defun t/workspace-overlay--on-window-buffer-change (&rest args)
  "Reapply overlay when WINDOW's buffer changes."
  (let ((window (t/workspace-overlay--window-from-args args)))
    (when (window-live-p window)
      (t/workspace-overlay--apply-window
       window
       (eq window (frame-selected-window (window-frame window)))))))

(defun t/workspace-overlay-refresh ()
  "Reapply workspace header line formatting to selected windows."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (t/workspace-overlay--clear-buffer-local)))
  (dolist (frame (frame-list))
    (let ((selected (frame-selected-window frame)))
      (dolist (window (window-list frame 'no-minibuf))
        (t/workspace-overlay--apply-window window
                                           (eq window selected)))))) 

(defun t/workspace-overlay--on-workspace-switch (&rest _)
  "Refresh overlay after workspace switches."
  ;; Defer so window configs restored by workspace switch are in place.
  (run-at-time 0 nil #'t/workspace-overlay-refresh))

(defun t/workspace-overlay--deferred-refresh (&rest _)
  "Refresh overlay after face/theme changes settle."
  (run-at-time 0 nil #'t/workspace-overlay-refresh))

(define-minor-mode t/workspace-overlay-mode
  "Show the current workspace in the top-right corner."
  :global t
  (if t/workspace-overlay-mode
      (progn
        (add-hook 'window-selection-change-functions
                  #'t/workspace-overlay--select-window)
        (add-hook 'window-buffer-change-functions
                  #'t/workspace-overlay--on-window-buffer-change)
        (add-hook 'window-configuration-change-hook
                  #'t/workspace-overlay-refresh)
        (add-hook '+workspace-switch-hook
                  #'t/workspace-overlay--on-workspace-switch)
        (add-hook 'after-load-theme-hook
                  #'t/workspace-overlay--deferred-refresh)
        (add-hook 'doom-after-reload-hook
                  #'t/workspace-overlay--deferred-refresh)
        (t/workspace-overlay-refresh))
    (remove-hook 'window-selection-change-functions
                 #'t/workspace-overlay--select-window)
    (remove-hook 'window-buffer-change-functions
                 #'t/workspace-overlay--on-window-buffer-change)
    (remove-hook 'window-configuration-change-hook
                 #'t/workspace-overlay-refresh)
    (remove-hook '+workspace-switch-hook
                 #'t/workspace-overlay--on-workspace-switch)
    (remove-hook 'after-load-theme-hook
                 #'t/workspace-overlay--deferred-refresh)
    (remove-hook 'doom-after-reload-hook
                 #'t/workspace-overlay--deferred-refresh)
    (dolist (frame (frame-list))
      (when-let ((window (frame-parameter frame 't/workspace-overlay-window)))
        (when (window-live-p window)
          (t/workspace-overlay--clear-window window))
        (set-frame-parameter frame 't/workspace-overlay-window nil)))
    ))

(t/workspace-overlay-mode 1)
