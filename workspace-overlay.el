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
  (let* ((window (or window (selected-window)))
         (buffer (window-buffer window))
         (workspace (t/workspace-overlay--name))
         (path (or (buffer-file-name buffer)
                   (buffer-name buffer)))
         (label (format "%s: %s" workspace path)))
    (propertize label
                't/workspace-overlay t
                'face '(:inherit header-line
                        :slant normal
                        :weight normal
                        :underline nil
                        :box nil
                        :foreground "#666666"
                        :height 0.8))))

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
  (and (stringp value)
       (text-property-any 0 (length value)
                          't/workspace-overlay t value)))

(defun t/workspace-overlay--blank ()
  "Return a blank header-line placeholder for inactive windows."
  (propertize " "
              't/workspace-overlay t
              'face '(:inherit header-line
                      :box nil
                      :height 0.8)))

(defvar-local t/workspace-overlay-prev-face-remap nil)

(defvar t/workspace-overlay--last-window nil)

(defun t/workspace-overlay--ensure-face-remap (buffer)
  "Ensure BUFFER uses a smaller header-line face."
  (with-current-buffer buffer
    (unless t/workspace-overlay-prev-face-remap
      (setq t/workspace-overlay-prev-face-remap face-remapping-alist))
    (setq face-remapping-alist
          (cons '(header-line (:height 0.8 :box nil))
                (assq-delete-all 'header-line face-remapping-alist)))))

(defun t/workspace-overlay--format (window active)
  "Return header line showing the current workspace at the right edge."
  (let* ((label (when active (t/workspace-overlay--label window)))
         (pad (when label (t/workspace-overlay--align window label)))
         (left (let ((prev (window-parameter window
                                             't/workspace-overlay-prev-header-line)))
                 (when (and prev (not (t/workspace-overlay--format-p prev)))
                   (format-mode-line prev nil window)))))
    (if active
        (concat (or left "") (or pad "") label)
      (if left
          (concat left (t/workspace-overlay--blank))
        (t/workspace-overlay--blank)))))

(defun t/workspace-overlay--clear-buffer-local ()
  "Clear stale buffer-local header-line overlays from prior versions."
  (when (t/workspace-overlay--format-p header-line-format)
    (setq header-line-format nil)))

(defun t/workspace-overlay--apply-window (window &optional active)
  "Apply workspace header line to WINDOW only."
  (let ((current (window-parameter window 'header-line-format))
        (buffer (window-buffer window)))
    (when (and (null (window-parameter window
                                       't/workspace-overlay-prev-header-line))
               (not (t/workspace-overlay--format-p current)))
      (set-window-parameter window
                            't/workspace-overlay-prev-header-line
                            current))
    (t/workspace-overlay--ensure-face-remap buffer)
    (set-window-parameter window
                          'header-line-format
                          (t/workspace-overlay--format window active))))

(defun t/workspace-overlay--clear-window (window)
  "Restore WINDOW's header line and clear overlay state."
  (let ((prev (window-parameter window
                                't/workspace-overlay-prev-header-line)))
    (set-window-parameter window 'header-line-format prev)
    (set-window-parameter window 't/workspace-overlay-prev-header-line nil)))

(defun t/workspace-overlay--select-window (window &rest _)
  "Show overlay in WINDOW for its frame."
  (let ((frame (window-frame window)))
    (set-frame-parameter frame 't/workspace-overlay-window window)
    (dolist (win (window-list frame 'no-minibuf))
      (t/workspace-overlay--apply-window win (eq win window)))))

(defun t/workspace-overlay--on-window-buffer-change (window _prev &rest _)
  "Reapply overlay when WINDOW's buffer changes."
  (t/workspace-overlay--apply-window
   window
   (eq window (frame-selected-window (window-frame window)))))

(defun t/workspace-overlay--post-command ()
  "Refresh overlay when window selection changes."
  (let ((current (selected-window)))
    (unless (eq current t/workspace-overlay--last-window)
      (setq t/workspace-overlay--last-window current)
      (t/workspace-overlay--select-window current))))

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
        (add-hook 'post-command-hook
                  #'t/workspace-overlay--post-command)
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
    (remove-hook 'post-command-hook
                 #'t/workspace-overlay--post-command)
    (remove-hook '+workspace-switch-hook
                 #'t/workspace-overlay--on-workspace-switch)
    (remove-hook 'after-load-theme-hook
                 #'t/workspace-overlay-refresh)
    (dolist (frame (frame-list))
      (when-let ((window (frame-parameter frame 't/workspace-overlay-window)))
        (when (window-live-p window)
          (t/workspace-overlay--clear-window window))
        (set-frame-parameter frame 't/workspace-overlay-window nil)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when t/workspace-overlay-prev-face-remap
          (setq face-remapping-alist t/workspace-overlay-prev-face-remap)
          (setq t/workspace-overlay-prev-face-remap nil))))))

(t/workspace-overlay-mode 1)
