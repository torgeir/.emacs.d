;;; -*- lexical-binding: t; -*-
(t/use-package doom-modeline
  :config
  (progn
    (setq doom-modeline-height 40)
    (t/add-hook-setq 'js2-mode-hook doom-modeline-env-command "node -v 2>&1")))

(t/use-package spacemacs-theme :defer t)
(t/use-package doom-themes
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (doom-themes-neotree-config)
    (doom-themes-visual-bell-config)))

(defun t-load-theme/config ()
  (defconst t-themes (list
                      'doom-one
                      'doom-vibrant
                      'doom-one-light) "Themes to cycle")

  (defun t/cycle-theme ()
    "Cycles themes in `t-themes'"
    (interactive)
    (let ((first (car t-themes))
          (rest (cdr t-themes)))
      (setq t-themes (append rest (list first)))
      (car t-themes)))

  (defun t/load-theme-cycle ()
    "Cycles `t-themes' and loads first theme in list"
    (interactive)
    (t/switch-theme (t/cycle-theme)))

  (defun t/load-theme ()
    "Loads theme and fixes fringe bg color"
    (interactive)
    (t/switch-theme (car t-themes)))

  (defadvice load-theme (after t/advice-after-load-theme activate)
    "Reset font size after loading theme"
    (t/reset-font-size))

  (t/add-hook 'after-init-hook 't/load-theme)

  (if has-gui
      (t/load-theme)
    (progn
      ;; load-theme after making the frame also when in terminal emacs
      (defvar *t-theme-did-load* nil)
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
          (t/load-theme))))))

(provide 't-load-theme)
