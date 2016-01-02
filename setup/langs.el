;; indents
(setq indent 2)
(setq indent-xml 4)

(let ((langs-extra-file (locate-user-emacs-file "langs-extra.el")))
  (when (file-exists-p langs-extra-file)
    (require 'langs-extra langs-extra-file)))

(t/declare-prefix "m" "Mode")

(use-package arduino-mode
  :defer t)

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package yaml-mode)

(use-package markdown-mode
  :defer t
  :mode "\\.\\(markdown\\|md\\)$"
  :config
  (bind-key "M-p" nil markdown-mode-map)
  (bind-key "M-n" nil markdown-mode-map))

(use-package css-mode
  :ensure nil
  :mode "\\.css$"
  :init
  (setq css-indent-offset indent)
  :config
  (bind-key "M-k" 't/css-kill-value css-mode-map)
  (dolist (fn '(turn-on-css-eldoc
                turn-on-smartparens-mode
                rainbow-mode))
    (add-hook 'css-mode-hook fn)))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :init
  (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package eldoc
  :defer t
  :diminish eldoc-mode)

(use-package less-css-mode
  :config
  (bind-key "M-k" 't/css-kill-value css-mode-map))

(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(javascript-jshint)))
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers '(json-jsonlist)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package flycheck-pos-tip
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; flycheck
(use-package flycheck-clojure
  :commands flycheck-mode
  :defer t
  :init
  (add-hook 'cider-mode-hook (lambda () (flycheck-mode 1)))

  :config
  (add-to-list 'flycheck-checkers 'clojure-cider-eastwood))

(use-package yasnippet
  :defer 1
  :config
  (defun js2-tab-properly ()
    (interactive)
    (let ((yas-fallback-behavior 'return-nil))
      (unless (yas-expand)
        (indent-for-tab-command)
        (if (looking-back "^\s*")
            (back-to-indentation))))))

;; js2-mode steals TAB, let's steal it back for yasnippet
(use-package js2-mode
  :commands  js2-mode
  :mode "\\.js$"
  :interpreter "node"
  :init
  ;; Let flycheck handle parse errors
  (setq-default js2-show-parse-errors nil
                js2-strict-missing-semi-warning nil
                js2-strict-inconsistent-return-warning nil
                js2-strict-var-hides-function-arg-warning nil
                js2-strict-missing-semi-warning nil
                js2-strict-trailing-comma-warning nil
                js2-strict-cond-assign-warning nil
                js2-strict-var-redeclaration-warning nil
                js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
  (setq-default js2-global-externs '("module" "require" "describe" "it" "sinon" "assert" "window" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset indent)
  (setq js-indent-level indent)

  :config
  (add-hook 'js2-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
  (add-hook 'js2-mode-hook 'tern-mode)

  (use-package nodejs-repl
    :defer t
    :config
    (bind-key "C-x C-e" #'t/send-region-to-nodejs-repl-process js2-mode-map)
    (evil-leader/set-key-for-mode 'js2-mode
      "mer" #'t/send-region-to-nodejs-repl-process))

  ;; don't steel keys
  (bind-key "M-j" 'nil js2-mode-map)
  (bind-key "M-." 'nil js2-mode-map)
  (bind-key "TAB" #'js2-tab-properly js2-mode-map)

  ;; jsx
  (use-package web-mode
    :mode "\\.\\(jsx\\)$"
    :config
    (bind-key "TAB" #'js2-tab-properly web-mode-map)
    (add-hook 'web-mode-hook ; http://web-mode.org/
              (lambda ()
                (dolist (mode '(js-mode html-mode css-mode))
                  (yas-activate-extra-mode mode))
                (setq web-mode-markup-indent-offset indent)
                (setq web-mode-css-indent-offset indent)
                (setq web-mode-code-indent-offset indent)))))

(t/declare-prefix "mr" "Refactor")

(use-package js2-refactor
  :config
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

(use-package json-mode
  :mode "\\.\\(json\\|jshintrc\\|eslintrc\\)$"
  :config
  (use-package json-reformat
    :defer t
    :commands json-reformat))

;; html
(use-package sgml-mode
  :ensure nil
  :init
  (set (make-local-variable 'sgml-basic-offset) indent-xml)

  :config
  ;; reindent after deleting tag with C-c DEL
  (defadvice sgml-delete-tag (after reindent activate)
    (indent-region (point-min) (point-max)))

  ;; nxml
  (add-hook 'nxml-mode-hook (setq nxml-child-indent indent-xml)))

(use-package simplezen
  :config
  (add-hook 'sgml-mode-hook (lambda ()
                              "make tab work, first try yasnippet, then simplezen"
                              (set (make-local-variable 'yas/fallback-behavior)
                                   '(apply simplezen-expand-or-indent-for-tab))))
  (bind-key "TAB" 'simplezen-expand-or-indent-for-tab html-mode-map))

(use-package clojure-mode
  :mode "\\.\\(edn\\|boot\\|clj\\|cljs\\)$"
  :config
  ;; stop nagging about saving
  (defadvice clojure-test-run-tests (before save-first activate) (save-buffer))
  (defadvice nrepl-load-current-buffer (before save-first activate) (save-buffer))
  (t/declare-prefix "mr" "Refactor"
                    ; https://github.com/clojure-emacs/clj-refactor.el/wiki
                    "?" 'cljr-describe-refactoring

                    "ar" 'cljr-add-require-to-ns
                    "ap" 'cljr-add-project-dependency

                    "cc" 'cljr-cycle-coll
                    "ct" 'cljr-cycle-thread
                    "ci" 'cljr-cycle-if

                    "dk" 'cljr-destructure-keys

                    "ec" 'cljr-extract-constant
                    "ed" 'cljr-extract-def
                    "el" 'cljr-expand-let
                    "ef" 'cljr-extract-function

                    "is" 'cljr-inline-symbol
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
                   ))


(use-package clojure-mode-extra-font-locking) ;; more syntax hilighting

(use-package cider
  :init
  ;; go to repl on connect
  (setq cider-repl-pop-to-buffer-on-connect nil)
  :config
  ;; minibuffer doc in repl
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  ;; match camel-case tokens
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode))

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

(use-package cloudformation-mode
  :ensure nil
  :load-path "site-lisp/cloudformation/"
  :commands cloudformation-mode
  :defer t)

;; ligatures
(defun ligature (tuple)
  "creates ligature"
  (lexical-let ((lexical-tuple tuple))
    (lambda ()
      (push lexical-tuple prettify-symbols-alist)
      (prettify-symbols-mode))))

(add-hook 'lisp-mode-hook (ligature '("lambda" . ?l)))
(add-hook 'lisp-interaction-mode (ligature '("lambda" . ?l)))
(add-hook 'emacs-lisp-mode-hook (ligature '("lambda" . ?l)))
(dolist (hook '(js2-mode-hook web-mode-hook))
  (add-hook hook (ligature '("function" . ?f))))

(defvar mode-line-cleaner-alist
  `((eldoc-mode . "")
    (paredit-mode . "")
    (rainbow-mode . "")
    (company-mode . "")
    (yas-minor-mode . "")
    (undo-tree-mode . "")
    (evil-escape-mode . "")
    (ethan-wspace-mode . "")
    (rainbow-delimiters-mode . "")
    (linum-relative-mode . "")
    (html-mode . "html")
    (js2-mode . "js2")
    (css-mode . "css")
    (less-css-mode . "less")
    (clojure-mode . "clj")
    (markdown-mode . "md")
    (emacs-lisp-mode . "el")
    (python-mode . "python"))
  "Alist for `t/clean-mode-line'. Modeline replacements")

(add-hook 'after-change-major-mode-hook 't/clean-mode-line)

(provide 'langs)
