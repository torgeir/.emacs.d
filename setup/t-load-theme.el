(defvar t-theme 'spacemacs-dark "Selected theme")
(use-package spacemacs-theme :defer t)

(defun t/tone-down-fringe-bg-color ()
  "Make fringe background-color the same as the background-color"
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(defun t/load-theme ()
  "Loads theme and fixes fringe bg color"
  (load-theme t-theme t))

(setq t-theme-did-load nil)

;; load-theme after making the frame also when in terminal emacs
(when (daemonp)
  (add-hook
   'after-make-frame-functions
   (lambda (frame)
     (unless t-theme-did-load
       (setq t-theme-did-load t)
       (with-selected-frame frame (t/load-theme)))
     ;; for some reason opening in terminal gives menu bar
     (menu-bar-mode -1))))

(defadvice server-create-window-system-frame
    (after t/advice-after-init-display activate)
  "Wait until server created window system frame before loading the theme"
  (unless t-theme-did-load
    (setq t-theme-did-load t)
    (t/load-theme)))

(defadvice load-theme (after t/advice-after-load-theme activate)
  "Tone down fringe after loading new themes"
  (t/tone-down-fringe-bg-color)
  (t/reset-font-size))

(provide 't-load-theme)
