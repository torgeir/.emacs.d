(t/use-package js2-mode
  ;; js2-mode steals TAB, let's steal it back for yasnippet
  :mode "\\.js$"
  :only-standalone t
  :interpreter "node"
  :init
  (progn
    ;; Let flycheck handle parse errors
    (setq-default js2-show-parse-errors nil
                  js2-strict-missing-semi-warning nil
                  js2-strict-inconsistent-return-warning nil
                  js2-strict-var-hides-function-arg-warning nil
                  js2-strict-cond-assign-warning nil
                  js2-strict-var-redeclaration-warning nil
                  js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
    (setq-default js2-global-externs '("module" "require" "describe" "it" "sinon" "assert" "window" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
    (setq-default js2-highlight-level 3
                  js-indent-level *t-indent*
                  js-switch-indent-offset *t-indent*
                  js2-basic-offset *t-indent*))    ;; don't steel keys
  :bind (:map
         js2-mode-map
         ("M-j" . nil)
         ("M-." . nil)
         ("TAB" . t/tab-properly))
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda ()
                               (flycheck-mode 1)
                               (turn-on-smartparens-mode)
                               (tern-mode)))

    (t/declare-prefix-for-mode 'js2-mode
                               "me" "Evaluate"
                               "b" 't/send-buffer-to-nodejs-repl-process
                               "r" 't/send-region-to-nodejs-repl-process)))

(t/use-package js2-refactor
  :only-standalone t
  :after js2-mode
  :init
  (t/declare-prefix-for-mode 'js2-mode "mr" "Refactor"
                             "ef" 'js2r-extract-function
                             "em" 'js2r-extract-method
                             "ev" 'js2r-extract-var
                             "ip" 'js2r-introduce-parameter
                             "iv" 'js2r-inline-var
                             "rv" 'js2r-rename-var

                             "ao" 'js2r-arguments-to-object
                             "co" 'js2r-contract-object
                             "eo" 'js2r-expand-object
                             "lp" 'js2r-localize-parameter
                             "tf" 'js2r-toggle-function-expression-and-declaration
                             "vt" 'js2r-var-to-this))

(t/use-package nodejs-repl
  :commands nodejs-repl
  :bind (:map
         js2-mode-map
         ("C-x C-e" . t/send-region-to-nodejs-repl-process))
  :config
  (progn
    (add-hook 'nodejs-repl-mode-hook
              (lambda ()
                (bind-key "C-d" 't/volatile-kill-buffer evil-insert-state-local-map)
                (bind-key "C-d" 't/volatile-kill-buffer evil-normal-state-local-map)))))

(provide 't-lang-js)