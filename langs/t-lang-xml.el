(use-package nxml-mode
  :ensure nil
  :mode "\\.\\(xml\\|svg\\|rss\\|xsd\\|xslt\\|plist\\)$"
  :init
  (setq sgml-basic-offset *t-indent-xml*)

  :config
  ;; reindent after deleting tag with C-c DEL
  (defadvice sgml-delete-tag (after reindent activate)
    (indent-region (point-min) (point-max)))

  ;; nxml
  (setq nxml-child-indent *t-indent-xml*))

(use-package simplezen
  :commands simplezen-expand-or-indent-for-tab
  :init
  (add-hook 'html-mode-hook (lambda ()
                              (bind-key "TAB" 't/tab-properly html-mode-map))))

(use-package tagedit
  :commands tagedit-mode
  :init
  (dolist (mode-hook '(html-mode-hook))
    (lexical-let ((mode-map (intern (replace-regexp-in-string "-hook" "-map" (format "%s" mode-hook)))))
      (add-hook mode-hook (lambda () (tagedit-mode 1)))
      (add-hook mode-hook (lambda ()
                            (eval `(bind-key "C-<left>"  'tagedit-forward-barf-tag ,mode-map))
                            (eval `(bind-key "C-<right>" 'tagedit-forward-slurp-tag ,mode-map))
                            (eval `(bind-key "C-k" 'tagedit-kill ,mode-map))
                            (eval `(bind-key "M-k" 'tagedit-kill-attribute ,mode-map))
                            (eval `(bind-key "M-r" 'tagedit-raise-tag ,mode-map))
                            (eval `(bind-key "M-s" 'tagedit-splice-tag ,mode-map))
                            (eval `(bind-key "M-S" 'tagedit-split-tag ,mode-map))
                            (eval `(bind-key "M-J" 'tagedit-join-tags ,mode-map)))))))


(provide 't-lang-xml)
