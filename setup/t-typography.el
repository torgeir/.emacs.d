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

;; highlight todos
(defface t-todo-face
  '((t (:foreground "white" :background "green" :bold t)))
  "Face used for highlighting todos" :group 'basic-faces)
(add-hook 'prog-mode-hook (lambda () (font-lock-add-keywords nil '(("\\(TODO\\)" 1 't-todo-face t)))))

(provide 't-typography)
