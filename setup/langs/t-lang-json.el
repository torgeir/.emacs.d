;;; -*- lexical-binding: t; -*-
(t/use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(t/use-package json-reformat
  :commands json-reformat)

(provide 't-lang-json)
