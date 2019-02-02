;;; -*- lexical-binding: t; -*-
(defun t-langs/config ()
  (t/declare-prefix "m" "Mode"))

(defconst t-langs '(t-lang-arduino
                    t-lang-applescript
                    t-lang-clojure
                    t-lang-css
                    t-lang-docker
                    t-lang-elisp
                    t-lang-elm
                    t-lang-flycheck
                    ;;t-lang-fsharp
                    t-lang-gitconfig
                    t-lang-gitignore
                    t-lang-haskell
                    t-lang-js
                    t-lang-json
                    t-lang-markdown
                    t-lang-pug
                    t-lang-php
                    t-lang-remark
                    t-lang-scala
                    t-lang-sh
                    t-lang-html
                    t-lang-yaml
                    t-lang-terraform
                    ))

(provide 't-langs)
