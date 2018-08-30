;;; -*- lexical-binding: t; -*-
(require 't-langs)
(defconst t-modules
  (remove-if 'null
             (append
              (list (when is-mac 't-mac)
                    (when is-linux 't-linux)
                    (when is-ms 't-cygwin)
                    't-load-theme
                    't-sane-defaults
                    't-custom
                    't-which-key
                    't-evil
                    't-vc
                    't-keys
                    't-typography
                    't-editor
                    't-shell
                    't-org
                    't-modeline
                    't-langs)
              t-langs)))

(provide 't-modules)
