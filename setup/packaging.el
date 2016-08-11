;; don't load outdated bytecode
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  ;; fetch the ones missing
  (setq use-package-always-ensure t)

  ;; lexical-let
  (require 'cl))
(defvar use-package-verbose t)
(require 'diminish)
(require 'bind-key)

(provide 'packaging)
