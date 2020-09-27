;;; -*- lexical-binding: t; -*-
(use-package json-mode
  :mode "\\(json\\|jshintrc\\|eslintrc\\)$")

(use-package json-reformat
  :commands json-reformat
  :init (setq json-reformat:indent-width 2))

(provide 't-lang-json)
