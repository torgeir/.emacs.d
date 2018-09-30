;;; -*- lexical-binding: t; -*-
(progn
  ;; site-lisp

  (use-package evil-use-package
    :ensure nil
    :load-path "site-lisp/evil-use-package/")

  (t/use-package cloudformation-mode
    :ensure nil
    :load-path "site-lisp/cloudformation/"
    :commands cloudformation-mode)

  (t/use-package helm-lines
    :commands helm-lines
    :ensure nil
    ;; TODO remove when https://github.com/melpa/melpa/pull/5300 is merged
    :load-path "~/Code/helm-lines.el/"
    )

  (t/use-package spotify
    :ensure nil
    :load-path "site-lisp/spotify/"
    :commands helm-spotify)


  (use-package doom-modeline
    :config
    (progn
      (setq doom-modeline-height 40)
      (t/add-hook-setq 'js2-mode-hook doom-modeline-env-command "node -v 2>&1")))

  (t/use-package nxml-eldoc
    :ensure nil
    :load-path "site-lisp/nxml-eldoc/"
    :commands turn-on-nxml-eldoc
    :init
    (t/add-hook 'nxml-mode-hook 'turn-on-nxml-eldoc))

  (t/use-package json-path-eldoc
    :ensure nil
    :load-path "site-lisp/json-path-eldoc/"
    :commands turn-on-json-path-eldoc
    :init
    (t/add-hook 'json-mode-hook 'turn-on-json-path-eldoc))

  (t/use-package html2text
    ;; needed for helm-google
    :commands helm-google
    :ensure nil
    :load-path "site-lisp/html2text"))

(progn
  ;; emacs local

  (t/use-package sgml-mode
    :ensure nil
    :commands html-mode
    :init
    (progn
      (t/add-hook-defun 'sgml-mode-hook t/hook-sgml
                        (set (make-local-variable 'sgml-basic-offset) *t-indent*))))

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
    :mode ("\\.\\(html|htm\\)" . html-mode)))

(provide 't-local)
