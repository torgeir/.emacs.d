;;; -*- lexical-binding: t; -*-
(t/use-package clojure-mode
  :pin melpa-stable
  :mode (("\\.\\(edn\\|boot\\|clj\\)$" . clojure-mode)
         ("\\.cljs$" . clojurescript-mode))
  :commands (clojure-mode clojurescript-mode)
  :config
  (progn
    (setq clojure-indent-style :align-arguments
          clojure-align-forms-automatically t)
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
    (defun t/init-clj-refactor (mode)
      (progn
        (clj-refactor-mode 1)
        (dolist (mapping '(("maps" . "outpace.util.maps")
                           ("seqs" . "outpace.util.seqs")
                           ("string" . "clojure.string")
                           ("reflect" . "clojure.reflect")
                           ("edn" . "clojure.edn")
                           ("time" . "clj-time.core")))
          (add-to-list 'cljr-magic-require-namespaces mapping t))

        (eval `(t/declare-prefix-for-mode ',mode
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
                                          ))))

    (t/add-hook 'clojure-mode-hook (t/lambda-i (t/init-clj-refactor 'clojure-mode)))
    (t/add-hook 'clojurescript-mode-hook (t/lambda-i (t/init-clj-refactor 'clojurescript-mode)))))


(t/use-package cljr-helm
  :commands cljr-helm)

(t/use-package clojure-mode-extra-font-locking
  :commands clojure-mode) ;; more syntax hilighting

(t/use-package cider
  :commands (cider cider-connect cider-jack-in)
  :init
  (progn
    (setq cider-boot-parameters "cider repl -s wait"
          cider-repl-display-help-banner nil
          cider-inject-dependencies-at-jack-in nil ;; theyre in ~/.boot/profile.boot
          cider-repl-pop-to-buffer-on-connect nil
          cider-overlays-use-font-lock t
          nrepl-hide-special-buffers t
          cider-prompt-for-symbol nil))
  :config
  (progn

    (bind-key "C-M-." 'cider-find-dwim cider-mode-map)

    (t/add-to-list 't-evil-major-modes '(cider-stacktrace-mode cider-docview-mode))

    (defun t/cider-insert-last-sexp-in-repl ()
      (interactive)
      (cider-insert-last-sexp-in-repl 't)
      (other-window 1))

    (defun t/init-clj-mode-keys-in-mode (mode)
      (t/declare-prefix-for-mode mode "d" "Mode"
                                 "f" 'cider-doc
                                 "j" 'cider-javadoc
                                 "a" 'cider-apropos)
      (t/declare-prefix-for-mode mode "m" "Mode"
                                 "j" 'cider-jack-in
                                 "J" 'cider-quit)
      (t/declare-prefix-for-mode mode "me" "Evaluate"
                                 "b" 'cider-eval-buffer
                                 "r" 'cider-eval-region
                                 "e" 't/cider-insert-last-sexp-in-repl
                                 "f" 'cider-eval-defun-at-point
                                 "R" 'cider-eval-last-sexp-and-replace))

    (t/init-clj-mode-keys-in-mode 'clojure-mode)
    (t/init-clj-mode-keys-in-mode 'clojurescript-mode)

    (t/add-hook 'cider-docview-mode-hook 'visual-line-mode)

    ;; minibuffer doc in repl
    (t/add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
    (t/add-hook 'cider-repl-mode-hook 'paredit-mode)
    (t/add-hook-defun 'cider--debug-mode-hook t/hook-cider-debug (evil-emacs-state))
    (t/add-hook 'cider-popup-buffer-mode-hook 'visual-line-mode)

    ;; company
    (t/after company
      (t/add-hook '(cider-repl-mode-hook cider-mode-hook) 'company-mode))

    ;; match camel-case tokens
    (add-hook 'clojure-mode-hook 'subword-mode)
    (add-hook 'clojure-mode-hook 'enable-paredit-mode)
    (add-hook 'clojurescript-mode-hook 'subword-mode)
    (add-hook 'clojurescript-mode-hook 'enable-paredit-mode)
    (t/add-hook 'clojure-mode-hook '(subword-mode enable-paredit-mode))))

(provide 't-lang-clojure)
