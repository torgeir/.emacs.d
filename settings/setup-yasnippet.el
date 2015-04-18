(require 'yasnippet)

;; use custom snippets
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))

;; on for all buffers
(yas-global-mode 1)

;; jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

;; inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; remove dropdowns
(setq yas-prompt-functions '(yas-ido-prompt yas-completing-prompt))

;; silence
(setq yas-verbosity 1)

;; wrap around region
(setq yas-wrap-around-region t)

(provide 'setup-yasnippet)
