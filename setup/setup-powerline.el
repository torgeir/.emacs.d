(use-package powerline
  :config
  (defface powerline-active-grey
    '((t (:foreground "#666" :background "#333" :weight bold :inherit mode-line)))
    "Powerline face 0."
    :group 'powerline)

  (defface powerline-active-blue
    '((t (:foreground "#98bcbd" :background "#333" :weight normal :inherit mode-line)))
    "Powerline face 0."
    :group 'powerline)

  (defface powerline-active
    '((t (:foreground "#bcba00" :background "#333" :weight bold :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface powerline-inactive
    '((t (:foreground "#444" :background "#333" :weight bold :inherit mode-line-inactive)))
    "Powerline face 0."
    :group 'powerline)

  (defun powerline-theme ()
    "Customisation of the default powerline theme"
    (interactive)
    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* (
                            (active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face-grey (if active 'powerline-active-grey 'powerline-inactive))
                            (face-blue (if active 'powerline-active-blue 'powerline-inactive))
                            (face-green (if active 'powerline-active 'powerline-inactive))
                            (evil-state-name (if (and (boundp 'evil-state)
                                                      evil-state)
                                                 (symbol-name evil-state) "emacs")) ;; default to "emacs" if evil is not loaded
                            (evil-state-name-short (capitalize (substring evil-state-name 0 1)))
                            (separator-left
                             (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (car powerline-default-separator-dir))))
                            (separator-right
                             (intern (format "powerline-%s-%s"
                                             powerline-default-separator
                                             (cdr powerline-default-separator-dir))))
                            (lhs (list
                                  (when (not (string-equal "emacs" evil-state-name))
                                    (powerline-raw (concat " " evil-state-name-short) face-blue))
                                  (powerline-raw "%*%*" face-grey 'l)
                                  (powerline-buffer-id face-green 'l)
                                  (when (and (boundp 'which-func-mode) which-func-mode)
                                    (powerline-raw which-func-format face-grey 'l))
                                  (when (boundp 'erc-modified-channels-object)
                                    (powerline-raw erc-modified-channels-object face-grey 'l))))
                            (rhs (list
                                  (when (not is-cygwin)
                                    (powerline-vc face-blue 'r))

                                  (powerline-raw global-mode-string face-grey 'r)
                                  (powerline-major-mode face-grey 'l)
                                  (powerline-process face-grey 'l)
                                  (powerline-minor-modes face-grey 'l)
                                  (powerline-narrow face-grey 'l)

                                  (powerline-raw " " face-grey)

                                  (powerline-raw "%l " face-grey)
                                  (powerline-raw ": " face-grey)
                                  (powerline-raw "%c " face-grey)
                                  (powerline-raw "%p " face-grey)
                                  (powerline-hud face-grey face-grey))))
                       (concat (powerline-render lhs)
                               (powerline-fill face-grey (powerline-width rhs))
                               (powerline-render rhs)))))))

  (powerline-theme))

(provide 'setup-powerline)
