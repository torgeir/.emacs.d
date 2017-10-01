;;; -*- lexical-binding: t; -*-
(require 't-modules)

(dolist (module t-modules)
  (require module)
  (t/call-init "%s/funcs" module)
  (t/call-init "%s/vars" module))

(dolist (pkg t-use-package-pkgs)
  (t/call-init "t/init-%s" pkg))

(dolist (module t-modules)
  (t/call-init "%s/config" module))

(provide 't-init)
