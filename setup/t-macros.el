;;; -*- lexical-binding: t; -*-
(require 'subr-x)

(defmacro t/lambda-i (&rest body)
  `(lambda () (interactive) ,@body))

(t/lambda-i)

(defmacro t/after (file-name &rest body)
  (declare (indent 1))
  `(if-let ((locate-library (symbol-name ',file-name)))
       (with-eval-after-load ',file-name ,@body)
     (user-error
      (format "t/after: for %s is not a filename in load-path?"
              (symbol-name ',file-name)))))

(defmacro t/when-ext (ext &rest body)
  "Run `body' when buffer's file has extension `ext'."
  (declare (indent 1))
  `(let ((ext-re (concat "\\." ,ext "$")))
     (when (and (buffer-file-name)
                (string-match ext-re (buffer-file-name)))
       ,@body)))

(defun t/ensure-list (i-or-is)
  "Ensure `i-or-is' is a list."
  (if (and (listp i-or-is)
           (not (functionp i-or-is)) ; lambda
           )
      i-or-is
    (list i-or-is)))

(defmacro t/add-hook (hook-or-hooks fn-or-fns &optional append local)
  "Add one or more hook fns."
  `(let ((hooks (t/ensure-list ,hook-or-hooks))
         (fns (t/ensure-list ,fn-or-fns)))
     (dolist (hook hooks)
       (dolist (fn fns)
         (add-hook hook fn ,append ,local)))))

(defmacro t/remove-hook (hook-or-hooks fn-or-fns)
  "Remove one or more hook fns"
  `(let ((hooks (t/ensure-list ,hook-or-hooks))
         (fns (t/ensure-list ,fn-or-fns)))
     (dolist (hook hooks)
       (dolist (fn fns)
         (remove-hook hook fn)))))

(defmacro t/add-hook-setq (hook-or-hooks var_ val_ &rest vars_)
  "A `setq' run in hooks."
  `(t/add-hook ,hook-or-hooks
               (lambda nil
                 (let ((var (quote ,var_))
                       (val (quote ,val_))
                       (vars (quote ,vars_))
                       (bindings '(setq)))
                   (while var
                     (setq bindings (cons val (cons var bindings))
                           var (and vars (pop vars))
                           val (and vars (pop vars))))
                   (eval (nreverse (copy-list bindings)))))))

(defmacro t/bind-in (maps_ key_ fn_ &rest bindings)
  "Bind keys in maps."
  `(let ((maps (t/ensure-list ,maps_))
         (key (quote ,key_))
         (fn (quote ,fn_))
         (bs (quote ,bindings)))
     (while key
       (dolist (map maps)
         (eval `(bind-key ,key ,fn ,map)))
       (setq key (and bs (pop bs))
             fn (and bs (pop bs))))))

(defmacro t/add-hook-defun (hook-or-hooks fn &rest body)
  "Create a defun `fn' with `body' in `hook-or-hooks'."
  `(progn
     (defun ,fn () (interactive) ,@body)
     (t/add-hook ,hook-or-hooks (quote ,fn))))

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

(defmacro t/safe-call (fn)
  "Expands to call `fn' only if it is bound to a function."
  `(when (fboundp (quote ,fn))
     (funcall (quote ,fn))))

(defmacro comment (&rest ignore)
  "Ignore stuff, return `nil'."
  nil)

(progn

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
             (body '()))

        ;; make :init and :config call defuns instead
        (setq entries (plist-put entries :init `(,vars-name)))
        (setq entries (plist-put entries :config `(,config-name)))

        ;; pass through some other use-package keys
        (let ((ks (list :if :init :config :mode :bind :ensure :diminish :after
                        :commands :defer :load-path :pin :evil-state)))
          (dolist (k ks)
            (when (plist-member entries k)
              (let ((v (plist-get entries k)))
                (setq body (plist-put body k v))))))

        (add-to-list 't-use-package-pkgs init-name t)
        (setq t-use-package-pkgs (delete-dups t-use-package-pkgs))

        `(progn
           (defun ,vars-name ()
             (interactive)
             (when *t-debug-init*
               (message "t/use-package vars: %s" (symbol-name ',vars-name)))
             ,init-body)
           (defun ,config-name ()
             (interactive)
             (when *t-debug-init*
               (message "t/use-package config: %s" (symbol-name ',config-name)))
             ,config-body)
           (defun ,init-name ()
             (interactive)
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
