;;; -*- lexical-binding: t; -*-
(t/use-package ensime
  :pin melpa-stable
  :commands (ensime ensime-mode)
  :config
  (progn
    (unbind-key "C-." evil-normal-state-map)
    (unbind-key "M-." evil-normal-state-map)
    (require 'ensime)
    (t/add-hook 'scala-mode-hook 'ensime-mode)
    (t/add-hook-defun 'scala-mode-hook t/hook-scala (bind-key "M-." 'ensime-edit-definition 'scala-mode-map))
    (t/declare-prefix-for-mode 'scala-mode "m" "Mode"
                               "j" 'ensime
                               "J" 'ensime-shutdown)
    (t/declare-prefix-for-mode 'scala-mode "me" "Evaluate"
                               "b" #'t/send-buffer-to-scala-repl
                               "r" #'t/send-region-to-scala-repl)))

(provide 't-lang-scala)
