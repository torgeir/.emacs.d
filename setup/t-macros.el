(defmacro t/macro-helm-ag-insert (thing fn)
  `(lambda ()
     (interactive)
     (setq-local helm-ag-insert-at-point ,thing)
     (,fn)
     (setq-local helm-ag-insert-at-point nil)))

(defmacro t/idle-timer (name fn every-minute)
  "Reloadable variant of run-with-idle-timer."
  `(progn
     (when (and (boundp ',name) ,name) (cancel-timer ,name))
     (setq ,name (run-with-idle-timer (* ,every-minute 60) t ,fn))))

(defmacro comment (&rest ignore) nil)

(progn

  (defvar t-use-package-t-layer-pkgs nil
    "List of packages inited by t/use-package that will be loaded
by spacemacs with the fake layer `t'.")
  (setq t-use-package-t-layer-pkgs nil)

  (defvar t-use-package-pkgs nil
    "List of all packages inited by t/use-package that will be used
for setting up vars and config after load")
  (setq t-use-package-pkgs nil)

  (defmacro t/use-package (package &optional key value &rest bindings)
    (declare (indent 1))
    (let* ((entries '())
           (init-name (intern (format "t/init-%s" package)))
           (vars-name (intern (format "t/vars-%s" package)))
           (config-name (intern (format "t/config-%s" package))))

      (while key
        (push value entries)
        (push key entries)
        (setq key (pop bindings)
              value (pop bindings)))

      (let* ((init-body (plist-get entries :init))
             (config-body (plist-get entries :config))
             (only-standalone (plist-get entries :only-standalone))
             (body '()))

        ;; make :init and :config call defuns instead
        (setq entries (plist-put entries :init `(,vars-name)))
        (setq entries (plist-put entries :config `(,config-name)))

        ;; pass through some other use-package keys
        (let ((ks (list :init :config :mode :bind :ensure :diminish :after :commands :defer :load-path :pin :evil-state)))
          (dolist (k ks)
            (when (plist-member entries k)
              (let ((v (plist-get entries k)))
                (setq body (plist-put body k v))))))

        ;; don't add :only-standalone packages in spacemacs t-"layer"
        (when (not only-standalone)
          (add-to-list 't-use-package-t-layer-pkgs package t)
          (setq t-use-package-t-layer-pkgs (delete-dups t-use-package-t-layer-pkgs)))

        (add-to-list 't-use-package-pkgs package t)
        (setq t-use-package-pkgs (delete-dups t-use-package-pkgs))

        `(progn
           (defun ,vars-name ()
             (when *t-debug-init*
               (message "t/use-package vars: %s" (symbol-name ',vars-name)))
             ,init-body)
           (defun ,config-name ()
             (when *t-debug-init*
               (message "t/use-package config: %s" (symbol-name ',config-name)))
             ,config-body)
           (defun ,init-name ()
             (when *t-debug-init*
               (message "t/use-package init: %s" (symbol-name ',init-name)))
             (use-package ,package ,@body)))))))

;; tests

(comment
 (plist-member '(:one nil :two 2) :one)
 (plist-member '(:one nil :two 2) :ensure)

 (pp (macroexpand-1 '(t/use-package wow
                       :ensure nil
                       :commands (winner-mode)
                       :bind (:map winner-mode-map ("C-c <left>" . winner-undo)))))
 )

(comment

 (t/use-package winner
   :bind (:map winner-mode-map ("C-c <left>" . winner-undo))
   :init (message "init")
   :config (message "config"))

 t-use-package-pkgs

 (symbol-function 't/vars-winner)
 (symbol-function 't/config-winner)
 (symbol-function 't/init-winner))

(comment
 (delete-dups '(1 2 3 1 2 3)))

(comment
 (t/use-package whaat
   :config
   (progn
     (message "config what one")
     (message "config what two"))
   :bind (+ 1 2))

 (symbol-function 't/vars-whaat)
 (symbol-function 't/config-whaat)
 (symbol-function 't/init-whaat))

(comment
 (symbol-function 't/config-which-key))

(defmacro t/def-pairs (pairs)
  "Create smartsmartparens wrapping function, e.g. t/wrap-with-paren"
  `(progn
     ,@(loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "t/wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(provide 't-macros)
