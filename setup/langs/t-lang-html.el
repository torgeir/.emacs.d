;;; -*- lexical-binding: t; -*-
(t/use-package simplezen
  :commands simplezen-expand-or-indent-for-tab)

(t/use-package tagedit
  :commands tagedit-mode
  :init
  ;; tagedit does not seem to work well with web-mode
  (t/add-hook-defun 'html-mode-hook t/hook-tagedit
                    (tagedit-mode 1)
                    (bind-key "C-<left>"  'tagedit-forward-barf-tag html-mode-map)
                    (bind-key "C-<right>" 'tagedit-forward-slurp-tag html-mode-map)
                    (bind-key "C-k" 'tagedit-kill html-mode-map)
                    (bind-key "M-k" 'tagedit-kill-attribute html-mode-map)
                    (bind-key "M-r" 'tagedit-raise-tag html-mode-map)
                    (bind-key "M-s" 'tagedit-splice-tag html-mode-map)
                    (bind-key "M-S" 'tagedit-split-tag html-mode-map)
                    (bind-key "M-J" 'tagedit-join-tags html-mode-map)))


(provide 't-lang-html)
