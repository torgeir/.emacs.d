(t/use-package js2-mode
  :mode "\\.js$"
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
                  js2-strict-trailing-comma-warning t ;; jshint does not warn about this now for some reason
                  js2-global-externs '("module" "require" "describe" "it" "window" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "process")
                  js2-highlight-level 3
                  js-indent-level *t-indent*
                  js-switch-indent-offset *t-indent*
                  js2-basic-offset *t-indent*)

    (t/add-hook-setq 'js2-mode-hook
                     ;; TODO tern-mode issue with question?
                     ;; tern capf seems to hijack tags? look for tags first:
                     completion-at-point-functions (reverse completion-at-point-functions))
    (t/add-hook 'js2-mode-hook #'(flycheck-mode
                                  turn-on-smartparens-mode
                                  js2-imenu-extras-mode
                                  tern-mode)))
  :config
  (progn
    (unbind-key "M-j" js2-mode-map)
    (unbind-key "M-." js2-mode-map)
    (bind-key "TAB" 't/tab-properly js2-mode-map)
    (t/declare-prefix-for-mode 'js2-mode
                               "me" "Evaluate"
                               "b" 't/send-buffer-to-nodejs-repl-process
                               "r" 't/send-region-to-nodejs-repl-process)))

(t/use-package prettier-js
  :init
  (progn
    (setq prettier-js-args nil)
    (defun t/prettier-jsx-hook ()
      (t/when-ext "jsx" (prettier-js-mode)))
    ;;(t/add-hook 'js2-mode-hook 'prettier-js-mode)
    ;;(t/add-hook 'web-mode-hook #'t/prettier-jsx-hook)
    ))

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
  :config
  (progn
    (bind-key "C-x C-e" 't/send-region-to-nodejs-repl-process js2-mode-map)
    (t/add-hook-defun 'nodejs-repl-mode-hook t/hook-nodejs-repl
                      (bind-key "C-d" 't/volatile-kill-buffer evil-insert-state-local-map)
                      (bind-key "C-d" 't/volatile-kill-buffer evil-normal-state-local-map))))

(t/use-package indium
  :commands (indium-repl-mode
             indium-interaction-mode
             indium-debugger-mode)
  :init
  (with-eval-after-load 'js2-mode
    (t/add-hook 'js2-mode-hook 'indium-interaction-mode)
    (t/declare-prefix-for-mode 'js2-mode
                               "m" "mode"
                               "j" 'indium-run-node
                               "J" 'indium-quit)
    (t/declare-prefix-for-mode 'js2-mode
                               "me" "Evaluate"
                               "b" 'indium-eval-buffer
                               "f" 'indium-eval-defun
                               "e" 'indium-eval-last-node))
  :config
  (progn
    (defun t/indium-debugger-mode-hook ()
      (bind-key "?" 'indium-debugger-show-help-message evil-normal-state-local-map)
      (bind-key "o" 'indium-debugger-step-over evil-normal-state-local-map)
      (bind-key "i" 'indium-debugger-step-into evil-normal-state-local-map)
      (bind-key "O" 'indium-debugger-step-out evil-normal-state-local-map)
      (bind-key "c" 'indium-debugger-resume evil-normal-state-local-map)
      (bind-key "l" 'indium-debugger-locals evil-normal-state-local-map)
      (bind-key "s" 'indium-debugger-stack-frames evil-normal-state-local-map)
      (bind-key "q" 'indium-debugger-resume evil-normal-state-local-map)
      (bind-key "h" 'indium-debugger-here evil-normal-state-local-map)
      (bind-key "e" 'indium-debugger-evaluate evil-normal-state-local-map)
      (bind-key "n" 'indium-debugger-next-frame evil-normal-state-local-map)
      (bind-key "p" 'indium-debugger-previous-frame evil-normal-state-local-map))
    (t/add-hook 'indium-debugger-mode-hook #'t/indium-debugger-mode-hook)
    (t/add-hook-defun 'indium-repl-mode-hook t/hook-indium-repl
                      (bind-key "C-d" 'indium-quit indium-repl-mode-map)
                      (bind-key "C-d" 'indium-quit evil-normal-state-local-map)
                      (bind-key "C-d" 'indium-quit evil-insert-state-local-map)
                      (bind-key "C-l" 'indium-repl-clear-output indium-repl-mode-map))
    (t/add-hook-defun 'indium-interaction-mode-hook t/hook-indium-interaction
                      (bind-key "C-c C-c" #'indium-eval-last-node evil-normal-state-local-map)
                      (bind-key "C-c C-c" #'indium-eval-last-node evil-insert-state-local-map))
    (defun t/indium-jk ()
      (bind-key "j" 'indium-inspector-next-reference evil-normal-state-local-map)
      (bind-key "k" 'indium-inspector-previous-reference evil-normal-state-local-map)
      (bind-key "q" 'quit-window evil-normal-state-local-map))
    (dolist (hook '(indium-repl-mode-hook
                    indium-debugger-frames-mode-hook
                    indium-debugger-locals-mode-hook
                    indium-inspector-mode-hook))
      (t/add-hook hook #'t/indium-jk))
    
    ;; inline evaled results when in js2-mode using cider
    (autoload 'cider--make-result-overlay "cider-overlays")
    (defun t/overlay-indium (r)
      (cider--make-result-overlay (indium-fontify-js r) :where (point) :duration 'command))
    (setq indium-interaction-eval-node-hook (list #'t/overlay-indium))))

(provide 't-lang-js)