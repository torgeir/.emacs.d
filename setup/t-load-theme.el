(use-package darktooth-theme :defer t)
(use-package spacemacs-theme :defer t)
(use-package gruvbox-theme :defer t)
(use-package doom-themes :defer t
  :init
  (add-hook 'find-file-hook 'doom-buffer-mode)
  (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)
  :config
  (require 'doom-neotree)
  (setq doom-neotree-enable-file-icons t))

(defvar t-themes (list
                  'doom-one
                  'spacemacs-dark
                  'darktooth
                  'spacemacs-light) "Themes to cycle")

(defun t/cycle-theme ()
  "Cycles themes in `t-themes'"
  (interactive)
  (let ((first (-take 1 t-themes))
        (rest (-drop 1 t-themes)))
    (setq t-themes (-concat rest first))
    (car (-take 1 t-themes))))

(defun t/tone-down-fringe-bg-color ()
  "Make fringe background-color the same as the background-color"
  (interactive)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(defun t/load-theme-cycle ()
  "Cycles `t-themes' and loads first theme in list"
  (interactive)
  (t/switch-theme (t/cycle-theme)))

(defun t/load-theme ()
  "Loads theme and fixes fringe bg color"
  (interactive)
  (t/switch-theme (nth 0 t-themes)))

(defvar *t-theme-did-load* nil)

;; load-theme after making the frame also when in terminal emacs
(when (daemonp)
  (add-hook
   'after-make-frame-functions
   (lambda (frame)
     (unless *t-theme-did-load*
       (setq *t-theme-did-load* t)
       (with-selected-frame frame (t/load-theme)))
     ;; for some reason opening in terminal gives menu bar
     (menu-bar-mode -1))))

(defadvice server-create-window-system-frame
    (after t/advice-after-init-display activate)
  "Wait until server created window system frame before loading the theme"
  (unless *t-theme-did-load*
    (setq *t-theme-did-load* t)
    (t/load-theme)))

(defadvice load-theme (after t/advice-after-load-theme activate)
  "Tone down fringe after loading new themes"
  (t/tone-down-fringe-bg-color)
  (t/reset-font-size))

(provide 't-load-theme)
