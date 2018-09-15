;;; -*- lexical-binding: t; -*-
(t/use-package js2-mode
  :mode "\\.jsx?$"
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

    (t/add-hook-defun 'js2-mode-hook t/js2-mode-hook
                      (flycheck-mode)
                      (turn-on-smartparens-mode)
                      (js2-imenu-extras-mode)
                      (tern-mode)
                      (t/add-company-backends 'company-web-html 'company-tern)))

  :config
  (progn
    (unbind-key "M-j" js2-mode-map)
    (unbind-key "M-." js2-mode-map)
    (t/declare-prefix-for-mode 'js2-mode
                               "h" "Help"
                               "h" 'tern-get-docs)
    (t/declare-prefix-for-mode 'js2-mode
                               "me" "Evaluate"
                               "b" 't/send-buffer-to-nodejs-repl-process
                               "r" 't/send-region-to-nodejs-repl-process)))

(t/use-package js2-refactor
  :after js2-mode
  :init
  (t/declare-prefix "mr" "Refactor"
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

(t/use-package cdnjs
  :commands (cdnjs-install-gocdnjs
             cdnjs-insert-url
             cdnjs-describe-package
             cdnjs-list-packages
             cdnjs-update-package-cache)
  :init
  (setq cdnjs-completing-read-function 'completing-read))

(t/use-package prettier-js
  :commands prettier-js-mode
  :init
  (progn
    (setq prettier-js-args '("--jsx-bracket-same-line")
          prettier-js-show-errors 'buffer)

    (defun t/prettier-hook ()
      (prettier-js-mode))

    (t/add-hook 'js-mode-hook #'t/prettier-hook)
    (t/add-hook 'js2-mode-hook #'t/prettier-hook)
    (t/add-hook 'css-mode-hook #'t/prettier-hook)
    (t/add-hook 'json-mode-hook #'t/prettier-hook)

    (defun t/disable-prettier ()
      (interactive)
      (prettier-js-mode -1))))

(t/use-package rjsx-mode
  :mode "\\.jsx?$"
  :commands (rjsx-mode))

(t/use-package indium
  :commands (indium-repl-mode
             indium-interaction-mode
             indium-debugger-mode)
  :init
  (t/after js2-mode
    (t/add-hook '(js2-mode-hook) 'indium-interaction-mode)
    (t/declare-prefix-for-mode 'js2-mode
                               "m" "mode"
                               "j" 'indium-run-node
                               "J" 'indium-quit)
    (t/declare-prefix-for-mode 'js2-mode
                               "me" "Evaluate"
                               "b" 'indium-eval-buffer
                               "f" 'indium-eval-defun
                               "e" 'indium-eval-last-node
                               "r" 'indium-eval-region))
  :config
  (progn

    (t/add-hook 'indium-inspector-mode-hook 'evil-emacs-state)
    (t/add-hook 'indium-debugger-mode-hook 'evil-emacs-state)
    (t/add-hook 'indium-debugger-locals-mode-hook 'evil-emacs-state)
    (t/add-hook 'indium-debugger-frames-mode-hook 'evil-emacs-state)

    (defun t/indium-eval ()
      (interactive)
      (save-excursion
        (evil-append-line 0)
        (call-interactively 'indium-eval-last-node)
        (sleep-for 0.001)
        (evil-normal-state nil)))

    (t/add-hook-defun 'indium-interaction-mode-hook t/hook-indium-interaction
                      (bind-key "C-c C-c" #'t/indium-eval evil-normal-state-local-map)
                      (bind-key "C-c C-c" #'t/indium-eval evil-insert-state-local-map))

    (t/add-hook-defun 'indium-repl-mode-hook t/hook-indium-repl
                      (bind-key "C-d" 'indium-quit indium-repl-mode-map)
                      (bind-key "C-d" 'indium-quit evil-normal-state-local-map)
                      (bind-key "C-d" 'indium-quit evil-insert-state-local-map)
                      (bind-key "C-l" 'indium-repl-clear-output indium-repl-mode-map))

    (autoload 'cider--make-result-overlay "cider-overlays")
    (defun t/overlay-indium (r)
      (cider--make-result-overlay (indium-fontify-js r) :where (point) :duration 'command))
    (setq indium-interaction-eval-node-hook (list #'t/overlay-indium))))

(t/use-package nodejs-repl
  :commands nodejs-repl
  :init
  (progn
    (t/after js2-mode
      (t/add-hook-defun '(js2-mode-hook) t/nodejs-repl-hook
                        (t/evil-ex-define-cmd-local "repl" 'nodejs-repl))))
  :config
  (progn
    (defun t/try-quit-nodejs-repl ()
      (interactive)
      (t/term-kill-if-finished 'comint-delchar-or-maybe-eof))
    (t/bind-in 'nodejs-repl-mode-map
               "C-d" #'t/try-quit-nodejs-repl)))

(provide 't-lang-js)
