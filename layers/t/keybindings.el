(dolist (module t-modules)
  (require module)
  (t/call-prefix ".*/setup" module))
