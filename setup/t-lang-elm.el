(t/use-package elm-mode
  :mode "\\.elm$"
  :init
  (progn
    (setq elm-tags-on-save t
          elm-format-on-save t))
  :config
  (progn
    (t/add-hook 'elm-mode-hook 'elm-oracle-setup-completion)
    (t/add-company-backend-hook 'elm-mode-hook 'company-elm)
    (t/declare-prefix-for-mode 'elm-mode "me" "Evaluate"
                               "b" (lambda ()
                                     (interactive)
                                     (elm-repl-load)
                                     (other-window -1))
                               "r" (lambda (start end)
                                     (interactive "r")
                                     (elm-repl-push start end)
                                     (other-window -1)))))

(t/use-package flycheck-elm
  :defer t
  :init
  (progn
    (t/add-hook 'flycheck-mode-hook 'flycheck-elm-setup t)))

(provide 't-lang-elm)
