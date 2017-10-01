(t/use-package json-mode
  :only-standalone t
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(t/use-package json-reformat
  :commands json-reformat)

(provide 't-lang-json)
