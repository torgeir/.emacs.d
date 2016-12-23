(use-package ensime
  :pin melpa-stable
  :commands (ensime ensime-mode)
  :init
  (require 'ensime)
  (add-hook 'scala-mode-hook 'ensime-mode)
  (add-hook 'scala-mode-hook (lambda ()
                               (bind-key "M-." 'ensime-edit-definition 'scala-mode-map)))
  :bind (:map
         evil-normal-state-map
         ("C-." . nil)
         ("M-." . nil))
  :config
  (t/declare-prefix-for-mode 'scala-mode "m" "Mode"
                             "j" 'ensime
                             "J" 'ensime-shutdown)
  (t/declare-prefix-for-mode 'scala-mode "me" "Evaluate"
                             "b" #'t/send-buffer-to-scala-repl
                             "r" #'t/send-region-to-scala-repl))

(provide 't-lang-scala)
