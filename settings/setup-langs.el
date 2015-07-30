(setq indent 2)

;; css
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'turn-on-css-eldoc "css-eldoc")

(setq css-indent-offset indent)

(add-hook 'css-mode-hook 'turn-on-css-eldoc)
(add-hook 'css-mode-hook 'turn-on-smartparens-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; js
;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(require 'js2-mode)
(autoload 'js2-mode "js2-mode" nil t)

(add-hook 'js2-mode-hook 'turn-on-smartparens-mode)
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

;; ligatures: function -> f, lambda -> λ
(add-hook 'js2-mode-hook
          (lambda ()
            (push '("function" . ?ƒ) prettify-symbols-alist)
            (prettify-symbols-mode)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("lambda" . ?λ) prettify-symbols-alist)
            (prettify-symbols-mode)))

(setq-default js2-global-externs '("module" "require" "describe" "it" "sinon" "assert" "window" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc$" . json-mode))
;;(define-key json-mode-map (kbd "C-c C-b") 'json-mode-beautify)

(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

;; jsx
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-hook 'web-mode-hook ; http://web-mode.org/
          (lambda ()
            (setq web-mode-markup-indent-offset indent)
            (setq web-mode-css-indent-offset indent)
            (setq web-mode-code-indent-offset indent)))

;; js2-mode steals TAB, let's steal it back for yasnippet
(require 'yasnippet)
(defun js2-tab-properly ()
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (indent-for-tab-command)
      (if (looking-back "^\s*")
          (back-to-indentation)))))

(define-key js2-mode-map (kbd "TAB") 'js2-tab-properly)
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "M-j") 'nil)
     (define-key js2-mode-map (kbd "M-.") 'nil)))

(setq js2-highlight-level 3)
(setq-default js2-basic-offset indent)
(setq js-indent-level indent)

;; html
(require 'sgml-mode)

;; reindent after deleting tag with C-c DEL
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(require 'simplezen)
(defun --setup-simplezen ()
  "make tab work, first try yasnippet, then simplezen"
  (set (make-local-variable 'yas/fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))
(add-hook 'sgml-mode-hook '--setup-simplezen)

(define-key html-mode-map (kbd "TAB") 'simplezen-expand-or-indent-for-tab)

;; sgml
(set (make-local-variable 'sgml-basic-offset) 4)
;; nxml
(add-hook 'nxml-mode-hook (setq nxml-child-indent 4))

;; clojure
(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

;; stop nagging about saving
(defadvice clojure-test-run-tests (before save-first activate) (save-buffer))
(defadvice nrepl-load-current-buffer (before save-first activate) (save-buffer))

;; more syntax hilighting
(require 'clojure-mode-extra-font-locking)

;; go to repl on connect
(setq cider-repl-pop-to-buffer-on-connect nil)

;; minibuffer doc in repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; match camel-case tokens
(add-hook 'clojure-mode-hook 'subword-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; flycheck and pos-tip
(require 'flycheck-clojure)
(add-hook 'cider-mode-hook (lambda () (flycheck-mode 1)))
(eval-after-load 'flycheck
  '(add-to-list 'flycheck-checkers
                'clojure-cider-eastwood))
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function
         #'flycheck-pos-tip-error-messages))

(provide 'setup-langs)