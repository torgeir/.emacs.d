;;; -*- lexical-binding: t; -*-

(require 'subr-x)

(defmacro comment (&rest ignore)
  "Ignore stuff, return `nil'."
  nil)
(defmacro t/lambda (&optional args &rest body)
  (declare (indent 1))
  (if body
      `(lambda ,args (interactive) ,@body)
    `(lambda (&optional &rest ignore) (interactive) ,args)))

(comment
 (t/lambda)
 (t/lambda 1)
 (t/lambda (one two) 1))

(defmacro t/after (file-name &rest body)
  (declare (indent 1))
  (if (locate-library (symbol-name file-name))
      `(with-eval-after-load ',file-name ,@body)
    ;;(message "t/after: for %s is not a filename in load-path?" file-name)
    ))

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
  (comment (dolist (hook (t/ensure-list (eval hook-or-hooks)))
             (unless (or (boundp hook) (listp hook))
               (message "%s is not a hook" hook))))
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
  (declare (indent 1))
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

(defmacro t/idle-timer (name fn every-minute)
  "Reloadable variant of run-with-idle-timer."
  `(progn
     (when (and (boundp ',name) ,name) (cancel-timer ,name))
     (setq ,name (run-with-idle-timer (* ,every-minute 60) t ,fn))))

(defmacro t/safe-call (fn)
  "Expands to call `fn' only if it is bound to a function."
  `(when (fboundp (quote ,fn))
     (funcall (quote ,fn))))

(defmacro t/def-pairs (pairs)
  "Create smartparens wrapping function, e.g. t/wrap-with-paren"
  `(progn
     ,@(cl-loop for (key . val) in pairs
             collect
             `(defun ,(read (concat
                             "t/wrap-with-"
                             (prin1-to-string key)
                             "s"))
                  (&optional arg)
                (interactive "p")
                (sp-wrap-with-pair ,val)))))

(provide 'macros)
