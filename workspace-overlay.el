(defun t/workspace-overlay--name ()
  "Return the current workspace name."
  (cond
   ((fboundp '+workspace-current-name)
    (+workspace-current-name))
   ((fboundp 'persp-name)
    (persp-name (get-current-persp)))
   (t "default")))

(defun t/workspace-overlay--label (&optional window)
  "Return the propertized label for the overlay in WINDOW."
  (let ((window (or window (selected-window))))
    (propertize (t/workspace-overlay--name)
                'face '(:inherit default
                        :slant normal
                        :weight normal
                        :underline nil
                        :foreground "#666666"
                        :height 0.65))))

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
                                    (1+ (t/workspace-overlay--label-px
                                         window label))))))
    (let* ((width (window-body-width window))
           (pad (max 0 (- width (1+ (string-width label))))))
      (make-string pad ?\s))))

(defun t/workspace-overlay--format-p (value)
  "Return non-nil if VALUE is this overlay's header-line format."
  (and (listp value)
       (equal value '(:eval (t/workspace-overlay--format)))))

(defun t/workspace-overlay--format ()
  "Return header line showing the current workspace at the right edge."
  (let* ((window (or (get-buffer-window (current-buffer) 0)
                     (selected-window)))
         (label (t/workspace-overlay--label window))
         (pad (t/workspace-overlay--align window label))
         (left (let ((prev (window-parameter window
                                             't/workspace-overlay-prev-header-line)))
                 (when (and prev (not (t/workspace-overlay--format-p prev)))
                   (format-mode-line prev nil window)))))
    (concat (or left "") pad label)))

(defun t/workspace-overlay--clear-buffer-local ()
  "Clear stale buffer-local header-line overlays from prior versions."
  (when (t/workspace-overlay--format-p header-line-format)
    (setq header-line-format nil)))

(defun t/workspace-overlay--apply-window (window)
  "Apply workspace header line to WINDOW only."
  (let ((current (window-parameter window 'header-line-format)))
    (when (and (null (window-parameter window
                                       't/workspace-overlay-prev-header-line))
               (not (t/workspace-overlay--format-p current)))
      (set-window-parameter window
                            't/workspace-overlay-prev-header-line
                            current))
    (set-window-parameter window
                          'header-line-format
                          '(:eval (t/workspace-overlay--format)))))

(defun t/workspace-overlay--clear-window (window)
  "Restore WINDOW's header line and clear overlay state."
  (let ((prev (window-parameter window
                                't/workspace-overlay-prev-header-line)))
    (set-window-parameter window 'header-line-format prev)
    (set-window-parameter window 't/workspace-overlay-prev-header-line nil)))

(defun t/workspace-overlay--select-window (window &rest _)
  "Show overlay only in WINDOW for its frame."
  (let* ((frame (window-frame window))
         (prev (frame-parameter frame 't/workspace-overlay-window)))
    (when (and (window-live-p prev) (not (eq prev window)))
      (t/workspace-overlay--clear-window prev))
    (set-frame-parameter frame 't/workspace-overlay-window window)
    (t/workspace-overlay--apply-window window)))

(defun t/workspace-overlay--on-window-buffer-change (window _prev &rest _)
  "Reapply overlay when WINDOW's buffer changes."
  (when (eq window (frame-parameter (window-frame window)
                                    't/workspace-overlay-window))
    (t/workspace-overlay--apply-window window)))

(defun t/workspace-overlay-refresh ()
  "Reapply workspace header line formatting to selected windows."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (t/workspace-overlay--clear-buffer-local)))
  (dolist (frame (frame-list))
    (t/workspace-overlay--select-window (frame-selected-window frame))))

(defun t/workspace-overlay--on-workspace-switch (&rest _)
  "Refresh overlay after workspace switches."
  ;; Defer so window configs restored by workspace switch are in place.
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
                  #'t/workspace-overlay-refresh)
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
                 #'t/workspace-overlay-refresh)
    (dolist (frame (frame-list))
      (when-let ((window (frame-parameter frame 't/workspace-overlay-window)))
        (when (window-live-p window)
          (t/workspace-overlay--clear-window window))
        (set-frame-parameter frame 't/workspace-overlay-window nil)))))

(t/workspace-overlay-mode 1)
