;;;###autoload
(defmacro t/idle-timer (name fn every-minute)
  "Reloadable variant of run-with-idle-timer."
  `(progn
     (when (and (boundp ',name) ,name) (cancel-timer ,name))
     (setq ,name (run-with-idle-timer (* ,every-minute 60) t ,fn))))

(provide 't-macros)
