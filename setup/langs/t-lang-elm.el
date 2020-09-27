;;; -*- lexical-binding: t; -*-
(use-package elm-mode
  :mode "\\.elm$"
  :init
  (progn
    (setq elm-tags-on-save t
          elm-format-on-save t
          elm-sort-imports-on-save t)
    (t/after dash-at-point
      (add-to-list 'dash-at-point-mode-alist '(elm-mode . "elm")))
    (t/after company
      (t/add-company-backends-hook 'elm-mode-hook 'company-elm))
    (t/add-hook 'elm-mode-hook 'elm-oracle-setup-completion))
  :config
  (t/declare-prefix-for-mode 'elm-mode "me" "Evaluate"
                             "b" (lambda ()
                                   (interactive)
                                   (elm-repl-load)
                                   (other-window -1))
                             "r" (lambda (start end)
                                   (interactive "r")
                                   (elm-repl-push start end)
                                   (other-window -1))))

(use-package flycheck-elm
  :commands flycheck-elm-setup
  :init
  (t/add-hook 'flycheck-mode-hook 'flycheck-elm-setup t))

(provide 't-lang-elm)
