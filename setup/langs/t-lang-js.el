;;; -*- lexical-binding: t; -*-
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

(provide 't-lang-js)
