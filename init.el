;;(package-initialize) stfu
(load (t/user-emacs-file "t-bootstrap.el"))
(require 't-packaging)

(t/timing-start)
(load (t/user-emacs-file "t-init.el"))
(unless (fboundp 'server-running-p) (require 'server))
(unless (server-running-p) (server-mode))
(t/timing-end)