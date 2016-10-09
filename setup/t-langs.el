;; indents
(setq indent 2)
(setq indent-xml 4)

(t/declare-prefix "m" "Mode")

(use-package arduino-mode
  :mode "\\.ino$"
  :commands arduino-mode
  :defer t)

(use-package gitconfig-mode
  :commands gitconfig-mode
  :mode "\\gitconfig$"
  :defer t)

(use-package gitignore-mode
  :commands gitignore-mode
  :defer t)

(use-package yaml-mode
  :commands yaml-mode
  :defer t)

(use-package pug-mode
  :defer t
  :mode "\\.pug$")

(use-package markdown-mode
  :defer t
  :mode "\\.\\(markdown\\|md\\)$"
  :bind (:map
         markdown-mode-map
         ("M-p" . nil)
         ("M-n" . nil)))

(use-package css-mode
  :ensure nil
  :mode "\\.css$"
  :bind (:map
         css-mode-map
         ("M-k" . t/css-kill-value))
  :init
  (setq css-indent-offset indent)
  :config
  (dolist (fn '(css-eldoc-enable
                turn-on-smartparens-mode
                rainbow-mode))
    (add-hook 'css-mode-hook fn)))

(use-package css-eldoc
  :commands css-eldoc-enable
  :init
  (add-hook 'css-mode-hook #'css-eldoc-enable))

(use-package eldoc
  :commands eldoc-mode
  :defer t
  ;;:diminish eldoc-mode
  )

(use-package less-css-mode
  :mode "\\.less$"
  :commands less-css-mode
  :bind (:map
         css-mode-map
         ("M-k" . t/css-kill-value)))

(use-package sh-script
  :mode ("\\.sh\\'" . sh-mode)
  :init
  (setq sh-indentation 2
        sh-basic-offset 2))

;; flycheck
(use-package flycheck
  :defer t
  ;; :diminish flycheck-mode
  :commands flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-pos-tip
  :defer t
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package flycheck-clojure
  :pin melpa-stable
  :commands flycheck-mode
  :defer t
  :init
  (add-hook 'cider-mode-hook (lambda () (flycheck-mode 1)))

  :config
  (add-to-list 'flycheck-checkers 'clojure-cider-eastwood))

(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1))

;; js2-mode steals TAB, let's steal it back for yasnippet
(use-package js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  ;; Let flycheck handle parse errors
  (setq-default js2-show-parse-errors nil
                js2-strict-missing-semi-warning nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
  (setq-default js2-global-externs '("module" "require" "describe" "it" "sinon" "assert" "window" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset indent)
  (setq js-indent-level indent)

  ;; don't steel keys
  :bind (:map
         js2-mode-map
         ("M-j" . nil)
         ("M-." . nil)
         ("TAB" . t/tab-properly))

  :config
  (add-hook 'js2-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'js2-mode-hook 'tern-mode)

  (t/declare-prefix-for-mode 'js2-mode
                             "me" "Evaluate"
                             "b" 't/send-buffer-to-nodejs-repl-process
                             "r" 't/send-region-to-nodejs-repl-process))

(use-package nodejs-repl
  :commands nodejs-repl
  :defer t
  :bind (:map
         js2-mode-map
         ("C-x C-e" . t/send-region-to-nodejs-repl-process)))

(use-package web-mode
  :mode "\\.jsx$"
  :init
  (setq web-mode-auto-close-style 2
        web-mode-enable-auto-quoting nil)
  (setq web-mode-markup-indent-offset indent
        web-mode-css-indent-offset indent
        web-mode-code-indent-offset indent)
  :config
  (bind-key "TAB" #'t/tab-properly web-mode-map)
  (add-hook 'web-mode-hook (lambda ()
                             (let ((is-jsx-file (not (null (string-match "\\.jsx$" (buffer-file-name))))))
                               (when is-jsx-file
                                 (js2-minor-mode)))))
  (add-hook 'web-mode-hook ; http://web-mode.org/
            (lambda ()
              (add-to-list 'company-dabbrev-code-modes 'web-mode)
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx"))
              (dolist (mode '(js-mode html-mode css-mode))
                (yas-activate-extra-mode mode)))))

(t/declare-prefix "mr" "Refactor")

(use-package js2-refactor
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

;; elm
(use-package elm-mode
  :mode "\\.elm$"
  :config
  (t/declare-prefix-for-mode 'elm-mode "me" "Evaluate"
                             "b" (lambda ()
                                   (interactive)
                                   (elm-repl-load)
                                   (other-window -1))
                             "r" (lambda (start end)
                                   (interactive "r")
                                   (elm-repl-push start end)
                                   (other-window -1))))
(use-package flycheck-elm
  :config
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-web-html))
  (with-eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-elm-setup)))

(use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(use-package json-reformat
  :commands json-reformat)

(use-package web-beautify
  :defer t
  :init
  (t/declare-prefix-for-mode 'web-mode  "m" "Mode" "=" 'web-beautify-html)
  (t/declare-prefix-for-mode 'css-mode  "m" "Mode" "=" 'web-beautify-js)
  (t/declare-prefix-for-mode 'json-mode "m" "Mode" "=" 'web-beautify-js)
  (t/declare-prefix-for-mode 'js2-mode  "m" "Mode" "=" 'web-beautify-js))

(use-package nxml-mode
  :ensure nil
  :mode "\\.\\(xml\\|svg\\|rss\\|xsd\\|xslt\\|plist\\)$"
  :init
  (setq sgml-basic-offset indent-xml)

  :config
  ;; reindent after deleting tag with C-c DEL
  (defadvice sgml-delete-tag (after reindent activate)
    (indent-region (point-min) (point-max)))

  ;; nxml
  (setq nxml-child-indent indent-xml))

(use-package simplezen
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


(use-package clojure-mode
  :pin melpa-stable
  :mode "\\.\\(edn\\|boot\\|clj\\|cljs\\)$"
  :commands (clojure-mode)
  :config
  ;; stop nagging about saving
  (defadvice clojure-test-run-tests (before save-first activate)
    (save-buffer))
  (defadvice nrepl-load-current-buffer (before save-first activate)
    (save-buffer))

  (use-package clj-refactor
    :pin melpa-stable
    :commands (clj-refactor-mode)
    :init
    (add-hook 'clojure-mode-hook
              (lambda ()
                (clj-refactor-mode 1)
                (dolist (mapping '(("maps" . "outpace.util.maps")
                                   ("seqs" . "outpace.util.seqs")
                                   ("string" . "clojure.string")
                                   ("reflect" . "clojure.reflect")
                                   ("edn" . "clojure.edn")
                                   ("time" . "clj-time.core")))
                  (add-to-list 'cljr-magic-require-namespaces mapping t))
                (t/declare-prefix "mr" "Refactor"
                                  ;; https://github.com/clojure-emacs/clj-refactor.el/wiki
                                  "?" 'cljr-describe-refactoring

                                  "ar" 'cljr-add-require-to-ns
                                  "ap" 'cljr-add-project-dependency
                                  "am" 'cljr-add-missing-libspec

                                  "cc" 'cljr-cycle-coll
                                  "ct" 'cljr-cycle-thread
                                  "ci" 'cljr-cycle-if

                                  "dk" 'cljr-destructure-keys

                                  "ec" 'cljr-extract-constant
                                  "ed" 'cljr-extract-def
                                  "el" 'cljr-expand-let
                                  "ef" 'cljr-extract-function

                                  "is" 'cljr-inline-symbol
                                  "in" 'clojure-insert-ns-form
                                  "un" 'clojure-update-ns
                                  "il" 'cljr-introduce-let

                                  "rr" 'cljr-remove-unused-requires
                                  "rl" 'cljr-remove-let
                                  "rs" 'cljr-rename-symbol
                                  "ru" 'cljr-replace-use

                                  "sn" 'cljr-sort-ns
                                  "sp" 'cljr-sort-project-dependencies
                                  "sr" 'cljr-stop-referring

                                  "th" 'cljr-thread
                                  "tf" 'cljr-thread-first-all
                                  "tl" 'cljr-thread-last-all

                                  "ua" 'cljr-unwind-all
                                  "uw" 'cljr-unwind

                                  "ml" 'cljr-move-to-let
                                  )))))

(use-package cljr-helm)

(use-package clojure-mode-extra-font-locking
  :commands clojure-mode) ;; more syntax hilighting

(use-package cider
  :pin melpa-stable
  :commands (cider cider-connect cider-jack-in)
  :init
  (setq cider-repl-pop-to-buffer-on-connect nil)
  :bind (:map
         cider-mode-map
         ("C-M-." . cider-find-dwim))
  :config
  (t/declare-prefix-for-mode 'clojure-mode "d" "Mode"
                             "f" 'cider-doc
                             "j" 'cider-javadoc
                             "a" 'cider-apropos)
  (t/declare-prefix-for-mode 'clojure-mode "m" "Mode"
                             "j" 'cider-jack-in
                             "J" 'cider-quit)
  (t/declare-prefix-for-mode 'clojure-mode "me" "Evaluate"
                             "b" 'cider-eval-buffer
                             "r" 'cider-eval-region
                             "f" 'cider-eval-defun-at-point
                             "R" 'cider-eval-last-sexp-and-replace)
  ;; minibuffer doc in repl
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider--debug-mode-hook (lambda () (interactive) (evil-local-mode 0)))

  ;; match camel-case tokens
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)

  ;; inline eval vars in elisp
  (autoload 'cider--make-result-overlay "cider-overlays")
  (defun endless/eval-overlay (value point)
    (cider--make-result-overlay (format "%S" value)
                                :where point
                                :duration 'command)
    value) ; preserve the return value
  (advice-add 'eval-region :around (lambda (f beg end &rest r) (endless/eval-overlay (apply f beg end r) end)))
  (advice-add 'eval-last-sexp :filter-return (lambda (r) (endless/eval-overlay r (point))))
  (advice-add 'eval-defun :filter-return (lambda (r) (endless/eval-overlay r (save-excursion (end-of-defun) (point))))))

;; lisp
(t/declare-prefix-for-mode 'lisp-interaction-mode "me" "Evaluate"
                           "b" 'eval-buffer
                           "e" 't/eval-region-or-last-sexp
                           "f" 'eval-defun
                           "r" 'eval-region
                           "R" 't/eval-and-replace)

(t/declare-prefix-for-mode 'lisp-mode "me" "Evaluate"
                           "b" 'eval-buffer
                           "e" 't/eval-region-or-last-sexp
                           "f" 'eval-defun
                           "r" 'eval-region
                           "R" 't/eval-and-replace)

(t/declare-prefix-for-mode 'emacs-lisp-mode "me" "Evaluate"
                           "b" 'eval-buffer
                           "e" 't/eval-region-or-last-sexp
                           "f" 'eval-defun
                           "r" 'eval-region
                           "R" 't/eval-and-replace)

(bind-key "TAB" #'t/tab-properly emacs-lisp-mode-map)

(use-package cloudformation-mode
  :ensure nil
  :load-path "site-lisp/cloudformation/"
  :commands cloudformation-mode
  :defer t)

(use-package fsharp-mode
  :mode "\\.fs[iylx]?$"
  :config
  (t/declare-prefix-for-mode 'fsharp-mode "me" "Evaluate"
                             "r" 'fsharp-eval-region))

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
(progn
  ;; ligatures
  (defun t/ligature (tuple)
    "creates ligature"
    (lexical-let ((lexical-tuple tuple))
      (lambda ()
        (push lexical-tuple prettify-symbols-alist)
        (prettify-symbols-mode))))

  (add-hook 'lisp-mode-hook (t/ligature '("lambda" . ?l)))
  (add-hook 'lisp-interaction-mode (t/ligature '("lambda" . ?l)))
  (add-hook 'emacs-lisp-mode-hook (t/ligature '("lambda" . ?l)))
  (dolist (hook '(js2-mode-hook web-mode-hook))
    (add-hook hook (t/ligature '("function" . ?f)))))

(defvar mode-line-cleaner-alist
  `(;(eldoc-mode . "")
    ;(paredit-mode . "")
    ;(rainbow-mode . "")
    ;(company-mode . "")
    ;(yas-minor-mode . "")
    ;(undo-tree-mode . "")
    (evil-escape-mode . "")
    (ethan-wspace-mode . "")
    ;(rainbow-delimiters-mode . "")
    (linum-relative-mode . "")
    (html-mode . "html")
    (js2-mode . "js2")
    (css-mode . "css")
    (less-css-mode . "less")
    (clojure-mode . "clj")
    (markdown-mode . "md")
    (emacs-lisp-mode . "el")
    (python-mode . "python")
    (tern-mode . "tern"))
  "Alist for `t/clean-mode-line'. Modeline replacements")

(add-hook 'after-change-major-mode-hook 't/clean-mode-line)

(provide 't-langs)
