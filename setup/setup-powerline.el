
(use-package powerline
  :config
  (defvar color-bg "#333")
  (defvar color-text-inactive "#444")
  (defvar color-text-active "#666")

  (defvar color-red "#f44")
  (defvar color-orange "#f94")
  (defvar color-yellow "#ff4")
  (defvar color-green "#bcba00")

  (defface powerline-active `((t (:foreground ,color-text-active :background ,color-bg :weight bold :inherit mode-line)))
    "Powerline face 0." :group 'powerline)
  (defface powerline-inactive `((t (:foreground ,color-text-inactive :background ,color-bg :weight bold :inherit mode-line-inactive)))
    "Powerline face 0." :group 'powerline)
  (defface powerline-active-blue `((t (:foreground "#98bcbd" :background ,color-bg :weight normal :inherit mode-line)))
    "Powerline face 0." :group 'powerline)
  (defface powerline-active-green `((t (:foreground ,color-green :background ,color-bg :weight bold :inherit mode-line)))
    "Powerline face 1." :group 'powerline)

  (defun powerline-flycheck-status ()
    (when flycheck-mode
      (defvar powerline-flycheck-error-face   `((t . (:background ,color-red))))
      (defvar powerline-flycheck-warning-face `((t . (:background ,color-orange))))
      (defvar powerline-flycheck-info-face    `((t . (:background ,color-yellow))))
      (defvar powerline-flycheck-success-face `((t . (:background ,color-green))))

      (require 'flycheck)
      (powerline-raw " "
                     (cond ((flycheck-has-current-errors-p 'error) powerline-flycheck-error-face)
                           ((flycheck-has-current-errors-p 'warning) powerline-flycheck-warning-face)
                           ((flycheck-has-current-errors-p 'info) powerline-flycheck-info-face)
                           (t powerline-flycheck-success-face))
                     'r)))

  (defun powerline-theme ()
    "Customisation of the default powerline theme"
    (interactive)

    (setq-default mode-line-format
                  '("%e"
                    (:eval
                     (let* (
                            (active (powerline-selected-window-active))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face-grey (if active 'powerline-active 'powerline-inactive))
                            (face-blue (if active 'powerline-active-blue 'powerline-inactive))
                            (face-green (if active 'powerline-active-green 'powerline-inactive))
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
                                  (powerline-flycheck-status)
                                  (powerline-hud face-grey face-grey))))
                       (concat (powerline-render lhs)
                               (powerline-fill face-grey (powerline-width rhs))
                               (powerline-render rhs)))))))

  (powerline-theme))

(provide 'setup-powerline)
