(use-package elm-mode
  :mode "\\.elm$"
  :init
  (setq elm-tags-on-save t
        elm-format-on-save t)
  :config
  (add-hook 'elm-mode-hook 'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm)
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
  :defer t
  :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup t))

(provide 't-lang-elm)