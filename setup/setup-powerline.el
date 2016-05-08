(use-package powerline
  :config
  (defvar color-red "#f44")
  (defvar color-orange "#fa4")
  (defvar color-yellow "#ff4")
  (defvar color-green "#acfa00")

  (defun powerline-flycheck-status ()
    (require 'flycheck)
    (when flycheck-mode
      (defvar powerline-flycheck-error-face   `((t . (:background ,color-red))))
      (defvar powerline-flycheck-warning-face `((t . (:background ,color-orange))))
      (defvar powerline-flycheck-info-face    `((t . (:background ,color-yellow))))
      (defvar powerline-flycheck-success-face `((t . (:background ,color-green))))
      (powerline-raw " "
                     (cond ((flycheck-has-current-errors-p 'error) powerline-flycheck-error-face)
                           ((flycheck-has-current-errors-p 'warning) powerline-flycheck-warning-face)
                           ((flycheck-has-current-errors-p 'info) powerline-flycheck-info-face)
                           (t powerline-flycheck-success-face))
                     'r)))

  (defun evil-state-name ()
    ;; default to "emacs" if evil is not loaded
    (let ((evil-state-name (if (and (boundp 'evil-state)
                                    evil-state)
                               (symbol-name evil-state)
                             "e")))
      (capitalize (substring evil-state-name 0 1))))

  (defface powerline-active-blue `((t (:foreground "#98bcbd" :weight normal :inherit powerline-active1)))
    "Custom highlight color used for some parts of the powerline" :group 'powerline)

  (defun t/git-branch ()
    (let ((has-git (vc-backend (buffer-file-name (current-buffer)))))
      (when has-git
        (let ((branch (vc-working-revision (buffer-file-name (current-buffer)))))
          (powerline-raw branch face-blue 'l)))))

  (defun t/create-powerline ()
    '("%e"
      (:eval
       (let* ((active (powerline-selected-window-active))
              (face-grey  (if active 'powerline-active1      'powerline-inactive1))
              (face-blue  (if active 'powerline-active-blue  'powerline-inactive1))

              (lhs (list
                    (powerline-flycheck-status)
                    (powerline-raw (format " %s" (evil-state-name)) face-blue)
                    (powerline-raw "%*%*" face-grey 'l)
                    (powerline-buffer-id face-grey 'l)
                    (when (not is-cygwin) (powerline-raw (t/git-branch) face-blue 'l))
                    (when (and (boundp 'which-function-mode) which-function-mode) (powerline-raw which-func-format face-grey 'l))
                    ))

              (rhs (list

                    (powerline-raw global-mode-string face-grey 'r)
                    (powerline-major-mode             face-grey 'l)
                    (powerline-process                face-grey 'l)
                    (powerline-minor-modes            face-grey 'l)
                    (powerline-narrow                 face-grey 'l)

                    (powerline-raw " "   face-grey)
                    (powerline-raw "%l " face-grey)
                    (powerline-raw ": "  face-grey)
                    (powerline-raw "%c " face-grey)

                    (powerline-hud 'cursor face-grey 1)
                    )))

         (concat (powerline-render lhs)
                 (powerline-fill face-grey (powerline-width rhs))
                 (powerline-render rhs))))))

  (defun t/update-powerline ()
    "powerline theme"
    (interactive)
    (setq mode-line-format nil)
    (setq mode-line-format (t/create-powerline))
    (force-mode-line-update 1))

  (setq-default mode-line-format (t/create-powerline)))

(provide 'setup-powerline)
