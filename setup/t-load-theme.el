(t/use-package darktooth-theme :defer t)

(t/use-package spacemacs-theme :defer t :only-standalone t)

(t/use-package gruvbox-theme :defer t)

(t/use-package doom-themes
  :defer t
  :config
  (progn
    (setq doom-enable-bold t         ; if nil, bolding are universally disabled
          doom-enable-italic t       ; if nil, italics are universally disabled

          ;; doom-one specific settings
          doom-one-brighter-modeline nil
          doom-one-brighter-comments t)

    ;; brighter minibuffer when active
    (add-hook 'minibuffer-setup-hook 'doom-brighten-minibuffer)

    ;; Enable nlinum line highlighting
    (doom-themes-nlinum-config)

    ;; Necessary for org-mode
    (setq org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks t)))

(defun t-load-theme/config ()
  (defconst t-themes (list
                      'doom-vibrant
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

  (defun t/load-theme-cycle ()
    "Cycles `t-themes' and loads first theme in list"
    (interactive)
    (t/switch-theme (t/cycle-theme)))

  (defun t/load-theme ()
    "Loads theme and fixes fringe bg color"
    (interactive)
    (t/switch-theme (nth 0 t-themes)))

  (defadvice load-theme (after t/advice-after-load-theme activate)
    "Tone down fringe after loading new themes"
    (t/reset-font-size))

  (add-hook 'after-init-hook (lambda () (t/load-theme)))

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
