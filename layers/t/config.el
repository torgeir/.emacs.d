(dolist (module t-modules)
  (require module))

(dolist (pkg t-use-package-pkgs)
  (t/call-init "t/vars-%s" pkg))
