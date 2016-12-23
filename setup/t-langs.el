(defvar *t-indent* 2)
(defvar *t-indent-xml* 4)

(t/declare-prefix "m" "Mode")

(dolist (lang '(t-lang-arduino
                t-lang-clojure
                t-lang-cloudformation
                t-lang-css
                t-lang-docker
                t-lang-elisp
                t-lang-elm
                t-lang-flycheck
                t-lang-fsharp
                t-lang-gitconfig
                t-lang-gitignore
                t-lang-haskell
                t-lang-js
                t-lang-json
                t-lang-markdown
                t-lang-pug
                t-lang-remark
                t-lang-scala
                t-lang-sh
                t-lang-web
                t-lang-xml
                t-lang-yaml))
  (require lang))

(provide 't-langs)
