(progn
  ;; ligatures
  (defun t/ligature (tuple)
    "creates ligature"
    (lexical-let ((lexical-tuple tuple))
      (lambda ()
        (push lexical-tuple prettify-symbols-alist)
        (prettify-symbols-mode))))

  (add-hook 'lisp-mode-hook (t/ligature '("lambda" . ?l)))
  (add-hook 'lisp-interaction-mode (t/ligature '("lambda" . ?l)))
  (add-hook 'emacs-lisp-mode-hook (t/ligature '("lambda" . ?l)))
  (dolist (hook '(js2-mode-hook web-mode-hook))
    (add-hook hook (t/ligature '("function" . ?f)))))

(progn
  ;; highlight TODO todos
  (defface t-todo-face
    '((t (:foreground "LightGrey" :background "DarkGreen" :bold t)))
    "Face used for highlighting todos" :group 'basic-faces)
  (add-hook 'prog-mode-hook (lambda () (font-lock-add-keywords nil '(("\\(TODO\\)" 1 't-todo-face t))))))

(progn
  (t/set-emoji-font nil) ; for when Emacs is started in GUI mode
  (add-hook 'after-make-frame-functions 't/set-emoji-font)) ; hook for when a frame is created with emacsclient

(provide 't-typography)