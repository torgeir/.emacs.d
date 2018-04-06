;;; -*- lexical-binding: t; -*-
(defun t-modeline/config ()

  (progn ; try without the modeline and the frame-title

    (setq t-old-modeline nil
          t-old-frametitle nil)

    (defun t/clean-frame ()
      "Remove mode-line and frame-title for a cleaner look."
      (interactive)
      (if t-old-modeline
          (progn
            (setq-default mode-line-format t-old-modeline frame-title-format t-old-frametitle)
            (setq         mode-line-format t-old-modeline frame-title-format t-old-frametitle)
            (setq t-old-modeline nil t-old-frametitle nil))
        (progn
          (setq t-old-modeline mode-line-format t-old-frametitle frame-title-format)
          (setq-default mode-line-format nil frame-title-format " ")
          (setq         mode-line-format nil frame-title-format " ")))))

  (defun t/init-modeline ()
    (when (not (or (equal frame-title-format "")
                   (equal frame-title-format " ")))
      (+doom-modeline|init)
      (when (not t-old-modeline) ;; first time around
        (t/clean-frame))))

  (t/add-hook 'after-init-hook #'t/init-modeline)

  (defvar mode-line-cleaner-alist
    `(;;(eldoc-mode . "")
      ;;(paredit-mode . "")
      ;;(rainbow-mode . "")
      ;;(company-mode . "")
      ;;(yas-minor-mode . "")
      (js2-mode . "js")
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
