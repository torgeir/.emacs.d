(t/use-package clojure-mode
  :pin melpa-stable
  :mode "\\.\\(edn\\|boot\\|clj\\|cljs\\)$"
  :commands (clojure-mode)
  :config
  (progn
    (setq clojure-indent-style :align-arguments)
    (put-clojure-indent '-> 1)
    (put-clojure-indent '->> 1)
    (put-clojure-indent 'doall 1)
    ;; stop nagging about saving
    (defadvice clojure-test-run-tests (before save-first activate)
      (save-buffer))
    (defadvice nrepl-load-current-buffer (before save-first activate)
      (save-buffer))))

(t/use-package clj-refactor
  :pin melpa-stable
  :commands (clj-refactor-mode)
  :init
  (progn
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
                (t/declare-prefix-for-mode 'clojure-mode
                                           "mr" "Refactor"
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

(t/use-package cljr-helm)

(t/use-package clojure-mode-extra-font-locking
  :commands clojure-mode) ;; more syntax hilighting

(t/use-package cider
  :pin melpa-stable
  :commands (cider cider-connect cider-jack-in)
  :init
  (progn
    (setq cider-boot-parameters "cider repl -s wait"
          cider-repl-pop-to-buffer-on-connect nil
          cider-overlays-use-font-lock t
          nrepl-hide-special-buffers t
          cider-prompt-for-symbol nil))
  :bind (:map
         cider-mode-map
         ("C-M-." . cider-find-dwim))
  :config
  (progn
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
    (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'cider--debug-mode-hook (lambda () (interactive) (evil-local-mode 0)))
    (add-hook 'cider-popup-buffer-mode-hook 'visual-line-mode)

    ;; company
    (with-eval-after-load 'company
      (add-hook 'cider-repl-mode-hook #'company-mode)
      (add-hook 'cider-mode-hook #'company-mode))

    ;; match camel-case tokens
    (add-hook 'clojure-mode-hook 'subword-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)))

(provide 't-lang-clojure)
