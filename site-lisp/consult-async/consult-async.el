;;; init.el -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl)
(require 'subr-x)
(require 'consult)

(defvar consult-t-history nil)

(defun consult-t-async (&optional fn)
  "Consult spotify by filter."
  (consult--read (consult-t--search-generator fn)
                 :prompt (format "Search: ")
                 ;;:lookup #'consult--lookup-member
                 ;;:category 'spotify-search-item
                 :history '(:input consult-t-history)
                 :initial (consult--async-split-initial "")
                 ;;:require-match t
                 ))

(defun consult-t--search-generator (fn)
  "Generate an async search closure for filter."
  (thread-first (consult--async-sink)
                (consult--async-refresh-immediate)
                (consult--async-map #'(lambda (n) (concat "" n)))
                (consult-t--async-search fn)
                (consult--async-throttle)
                (consult--async-split)))

(defun consult-t--async-search (next fn)
  "Async search with NEXT and FILTER."
  (let ((current ""))
    (lambda (action)
      (pcase action
        ((pred stringp) (if (string-empty-p action)
                            (funcall next 'flush)
                          (when action
                            (funcall next 'flush)
                            (funcall fn action (lambda (res)
                                                 (funcall next (remove-if
                                                                (lambda (n)
                                                                  (or (null n) (string-empty-p n)))
                                                                res)))))))
        (_ (funcall next action))))))

(message "result: %s" (consult-t-async (lambda (action fn)
                                       (message "%s" (type-of fn))
                                       (t/async-shell-command
                                        "file listing"
                                        (concat "ls -l " action)
                                        (lambda (p code res)
                                          (funcall fn res))))))
