;;; -*- lexical-binding: t; -*-
(defun t-typography/config ()

  (t/add-hook-defun '(web-mode-hook) t/ligatures
                    (push '("function" . ?f) prettify-symbols-alist)
                    (prettify-symbols-mode))

  (t/add-hook-defun 'prog-mode-hook t/hook-prog
                    (defface t-todo-face
                      '((t (:foreground "LightGrey" :background "DarkGreen" :bold t)))
                      "Face used for highlighting todos" :group 'basic-faces)
                    (font-lock-add-keywords nil '(("\\(TODO\\)" 1 't-todo-face t))))

  (progn
    (t/set-emoji-font nil) ; for when Emacs is started in GUI mode
    (t/add-hook 'after-make-frame-functions 't/set-emoji-font))) ; hook for when a frame is created with emacsclient

;; add window margins
(setq-default left-margin-width 1
              right-margin-width 1)
(set-window-buffer nil (current-buffer))

(provide 't-typography)
