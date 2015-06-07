;; css
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(autoload 'turn-on-css-eldoc "css-eldoc")

(setq css-indent-offset 2)

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
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'turn-on-smartparens-mode)
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jshintrc$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.eslintrc$" . javascript-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

(require 'flycheck)
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                          '(json-jsonlist)))

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
  '(define-key js2-mode-map (kbd "M-j") 'nil))

(setq-default js2-basic-offset 2)
(setq js2-highlight-level 3)
(setq js-indent-level 2)

;; html
(require 'sgml-mode)

;; reindent after deleting tag with C-c DEL
(defadvice sgml-delete-tag (after reindent-buffer activate)
  (cleanup-buffer))

(require 'simplezen)
(defun --setup-simplezen ()
  "make tab work, first try yasnippet, then simplezen"
  (set (make-local-variable 'yas/fallback-behavior)
       '(apply simplezen-expand-or-indent-for-tab)))
(add-hook 'sgml-mode-hook '--setup-simplezen)

(define-key html-mode-map (kbd "TAB") 'simplezen-expand-or-indent-for-tab)

;; nxml

(add-hook 'nxml-mode-hook (setq nxml-child-indent 4))

;; clojure
(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))

;; paredit
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; match camel-case tokens
(add-hook 'clojure-mode-hook 'subword-mode)

;; more syntax hilighting
(require 'clojure-mode-extra-font-locking)

;; go to repl on connect
(setq cider-repl-pop-to-buffer-on-connect t)

;; minibuffer doc in repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; paredit in repl
(add-hook 'cider-repl-mode-hook 'paredit-mode)


(provide 'setup-langs)
