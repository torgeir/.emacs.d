;;; -*- lexical-binding: t; -*-
(t/use-package doom-modeline
  :config
  (progn
    (setq doom-modeline-height 40
          doom-themes-padded-modeline 1)
    (t/add-hook-setq 'js2-mode-hook doom-modeline-env-command "node -v 2>&1")))

(t/use-package doom-themes
  :config
  (progn
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (doom-themes-visual-bell-config)
    (t/add-hook 'org-mode-hook 'doom-themes-org-config)
    (t/add-hook 'neotree-mode-hook 'doom-themes-neotree-config)))

(defun t-load-theme/config ()
  (defconst t-themes (list
                      'doom-one
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

  (defun t/reset-font-after-load (&rest args) (interactive) (t/reset-font-size))
  (advice-add 'load-theme :after 't/reset-font-after-load)

  (defvar *t-theme-did-load* nil)
  (defun t/load-theme-once ()
    (unless *t-theme-did-load*
      (setq *t-theme-did-load* t)
      (t/load-theme)))
  (t/add-hook 'after-init-hook
              (lambda ()
                (if has-gui
                    (t/load-theme)
                  (progn
                    ;; load-theme after making the frame also when in terminal emacs
                    (when (daemonp)
                      (add-hook 'after-make-frame-functions
                                (lambda (frame)
                                  (with-selected-frame frame (t/load-theme-once))
                                  ;; for some reason opening in terminal gives menu bar
                                  (menu-bar-mode -1))))
                    (advice-add server-create-window-system-frame :after 't/load-theme-once))))))

(provide 't-load-theme)
