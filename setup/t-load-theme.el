(defvar t-theme 'spacemacs-dark "Selected theme")
(use-package spacemacs-theme :defer t)

(defun t/tone-down-fringe-bg-color ()
  "Make fringe background-color the same as the background-color"
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(defun t/load-theme ()
  "Loads theme and fixes fringe bg color"
  (load-theme t-theme t)
  (t/tone-down-fringe-bg-color))

;; load-theme after making the frame also when in terminal emacs
(when (daemonp)
  (add-hook
   'after-make-frame-functions
   (lambda (frame)
     (with-selected-frame frame
       (t/load-theme)))))

(defadvice server-create-window-system-frame
    (after t/advice-after-init-display activate)
  "Wait until server created window system frame before loading the theme"
  (t/load-theme)
  (ad-disable-advice 'server-create-window-system-frame 'after 't/advice-after-init-display)
  (ad-activate 'server-create-window-system-frame))

(provide 't-load-theme)
