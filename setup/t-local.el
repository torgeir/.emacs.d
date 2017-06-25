(progn
  ;; site-lisp
  
  (t/use-package evil-use-package
    :ensure nil
    :load-path "site-lisp/evil-use-package/")

  (t/use-package cloudformation-mode
    :ensure nil
    :load-path "site-lisp/cloudformation/"
    :commands cloudformation-mode)

  (t/use-package helm-insert-line-from-project
    :commands t/helm-find-and-insert-line-from-project
    :ensure nil
    :load-path "site-lisp/helm-insert-line-from-project/")

  (t/use-package spotify
    :ensure nil
    :load-path "site-lisp/spotify/"
    :commands helm-spotify)

  (t/use-package t-doom-modeline
    :ensure nil
    :load-path "site-lisp/t-doom-modeline/"))

(progn
  ;; emacs local

  (t/use-package sgml-mode
    :ensure nil
    :init
    (progn
      (add-hook 'sgml-mode-hook
                (lambda ()
                  (set (make-local-variable 'sgml-basic-offset) *t-indent-xml*)))))

  (t/use-package nxml-mode
    :ensure nil
    :mode "\\.\\(xml\\|svg\\|rss\\|xsd\\|xslt\\|plist\\)$"
    :config
    ;; reindent after deleting tag with C-c DEL
    (defadvice sgml-delete-tag (after reindent activate)
      (indent-region (point-min) (point-max)))

    ;; nxml
    (setq nxml-child-indent *t-indent-xml*))

  (t/use-package html
    :ensure nil
    :mode ("\\.\\(html|htm\\)" . html-mode))

  )

(provide 't-local)
