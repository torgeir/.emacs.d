;; intents
(setq indent 2)
(setq indent-xml 4)

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
  (bind-key "M-k" 'css-kill-value css-mode-map)
  (dolist (fn '(turn-on-css-eldoc
                turn-on-smartparens-mode
                rainbow-mode))
    (add-hook 'css-mode-hook fn)))

(use-package css-eldoc
  :commands turn-on-css-eldoc
  :init
  (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package less-css-mode
  :config
  (bind-key "M-k" 'css-kill-value css-mode-map))

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
  (setq-default js2-show-parse-errors nil)
  (setq-default js2-strict-missing-semi-warning nil)
  (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
  (setq-default js2-global-externs '("module" "require" "describe" "it" "sinon" "assert" "window" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

  (setq js2-highlight-level 3)
  (setq-default js2-basic-offset indent)
  (setq js-indent-level indent)

  :config
  (add-hook 'js2-mode-hook 'turn-on-smartparens-mode)
  (add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

  (use-package nodejs-repl
    :defer t
    :config
    (bind-key "C-x C-e" #'send-region-to-nodejs-repl-process js2-mode-map)
    (evil-leader/set-key-for-mode 'js2-mode
      "mer" #'send-region-to-nodejs-repl-process))

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
                (setq web-mode-code-indent-offset indent))))

  (use-package json-mode
    :mode "\\.\\(json\\|jshintrc\\|eslintrc\\)$"
    :config
    (use-package json-reformat)))

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
  (defadvice nrepl-load-current-buffer (before save-first activate) (save-buffer)))


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

;; elisp
(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "meb" 'eval-buffer
  "mec" 'eval-current-buffer
  "mee" 'eval-last-sexp
  "mef" 'eval-defun
  "mer" 'eval-region)

;; ligatures
(require 'cl)
(defun ligature (tuple)
  "creates ligature"
  (lexical-let ((lexical-tuple tuple))
    (lambda ()
      (push lexical-tuple prettify-symbols-alist)
      (prettify-symbols-mode))))

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
    (rainbow-delimiters-mode . "")
    (html-mode . "html")
    (js2-mode . "js2")
    (css-mode . "css")
    (less-css-mode . "less")
    (clojure-mode . "clj")
    (markdown-mode . "md")
    (emacs-lisp-mode . "el")
    (python-mode . "python"))
  "Alist for `clean-mode-line'. Modeline replacements")

(add-hook 'after-change-major-mode-hook 'clean-mode-line)

(provide 'langs)
