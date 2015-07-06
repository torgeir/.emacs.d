(require 'dired)

(put 'dired-find-alternate-file 'disabled nil)

;; jump to dir containing file
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
(global-set-key (kbd "C-x M-j") '(lambda () (interactive) (dired-jump 1)))

(define-key dired-mode-map (kbd "M-<up>") (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map (kbd "M-p") (lambda () (interactive) (find-alternate-file "..")))
(define-key dired-mode-map (kbd "M-<down>") (lambda () (interactive) (dired-find-alternate-file)))
(define-key dired-mode-map (kbd "M-n") (lambda () (interactive) (dired-find-alternate-file)))

;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

(provide 'setup-dired)
