;; FIXME woot?
(t/use-package hl-todo
  :defer 1
  :config (global-hl-todo-mode))

;;; -*- lexical-binding: t; -*-
(defun t-typography/config ()

  (t/add-hook-defun '(js2-mode-hook) t/ligatures
                    (push '("function" . ?ƒ) prettify-symbols-alist)
                    (prettify-symbols-mode))

  (progn
    (t/set-emoji-font nil) ; for when Emacs is started in GUI mode
    (t/add-hook 'after-make-frame-functions 't/set-emoji-font))) ; hook for when a frame is created with emacsclient

;; add window margins
(setq-default left-margin-width 1
              right-margin-width 1)
(set-window-buffer nil (current-buffer))

(provide 't-typography)
