;;; -*- lexical-binding: t; -*-
(t/use-package simplezen
  :commands simplezen-expand-or-indent-for-tab)

(t/use-package tagedit
  :commands tagedit-mode
  :init
  ;; tagedit does not seem to work well with web-mode
  (t/add-hook-defun 'html-mode-hook t/hook-tagedit
                    (tagedit-mode 1)
                    (t/bind-in 'html-mode-map
                      "C-<left>"  'tagedit-forward-barf-tag
                      "C-<right>" 'tagedit-forward-slurp-tag
                      "C-k" 'tagedit-kill
                      "M-k" 'tagedit-kill-attribute
                      "M-r" 'tagedit-raise-tag
                      "M-s" 'tagedit-splice-tag
                      "M-S" 'tagedit-split-tag
                      "M-J" 'tagedit-join-tags)))

(provide 't-lang-html)
