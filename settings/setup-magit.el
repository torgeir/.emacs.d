(setq magit-last-seen-setup-instructions "1.4.0")

(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; make magit run full screen
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'setup-magit)
