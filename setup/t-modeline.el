;;; -*- lexical-binding: t; -*-
(defun t-modeline/config ()

  (defun t/init-modeline () (+doom-modeline|init))
  (t/add-hook 'after-init-hook #'t/init-modeline)

  (defvar mode-line-cleaner-alist
    `(;;(eldoc-mode . "")
      ;;(paredit-mode . "")
      ;;(rainbow-mode . "")
      ;;(company-mode . "")
      ;;(yas-minor-mode . "")
      ;;(undo-tree-mode . "")
      (evil-escape-mode . "")
      (ethan-wspace-mode . "")
      ;;(rainbow-delimiters-mode . "")
      (html-mode . "html")
      (css-mode . "css")
      (less-css-mode . "less")
      (clojure-mode . "clj")
      (markdown-mode . "md")
      (emacs-lisp-mode . "el")
      (python-mode . "python")
      (tern-mode . "tern"))
    "Alist for `t/clean-mode-line'. Modeline replacements")

  (t/add-hook 'after-change-major-mode-hook 't/clean-mode-line))

(provide 't-modeline)
