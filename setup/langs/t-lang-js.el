;;; -*- lexical-binding: t; -*-

(t/use-package prettier-js
  :commands prettier-js-mode
  :init
  (progn
    (setq prettier-js-args nil)
    (defun t/prettier-jsx-hook ()
      (t/when-ext "jsx" (prettier-js-mode)))
    (defun t/enable-prettier ()
      (interactive)
      (prettier-js-mode 1)
      (t/add-hook 'web-mode-hook #'t/prettier-jsx-hook))
    (defun t/disable-prettier ()
      (interactive)
      (prettier-js-mode -1)
      (t/remove-hook 'web-mode-hook #'t/prettier-jsx-hook))))

(t/use-package nodejs-repl
  :commands nodejs-repl
  :config
  (progn
    (bind-key "C-x C-e" 't/send-region-to-nodejs-repl-process web-mode-map)
    (t/add-hook-defun 'nodejs-repl-mode-hook t/hook-nodejs-repl
                      (bind-key "C-d" 't/volatile-kill-buffer evil-insert-state-local-map)
                      (bind-key "C-d" 't/volatile-kill-buffer evil-normal-state-local-map))))

(t/use-package indium
  :commands (indium-repl-mode
             indium-interaction-mode
             indium-debugger-mode)
  :init
  (with-eval-after-load 'web-mode
    (t/add-hook 'web-mode-hook 'indium-interaction-mode)
    (t/declare-prefix-for-mode 'web-mode
                               "m" "mode"
                               "j" 'indium-run-node
                               "J" 'indium-quit)
    (t/declare-prefix-for-mode 'web-mode
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

    (autoload 'cider--make-result-overlay "cider-overlays")
    (defun t/overlay-indium (r)
      (cider--make-result-overlay (indium-fontify-js r) :where (point) :duration 'command))
    (setq indium-interaction-eval-node-hook (list #'t/overlay-indium))))

(provide 't-lang-js)
