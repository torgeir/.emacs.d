;;; -*- lexical-binding: t; -*-
(defconst t-debug-timer-threshold 0.2 "Threshold value for when to debug load info")
(defconst t-buffer-load-times "*load-times*" "Buffer name for the load times buffer")

(when *t-debug-init*

  (setq use-package-debug t)

  (with-current-buffer (get-buffer-create t-buffer-load-times)
    (insert (format "Threshold set at %.3f seconds\n\n"
                    t-debug-timer-threshold)))

  (defadvice package-initialize (around t/advice-package-initalize activate)
    (let ((start (current-time)) res delta)
      (setq res ad-do-it
            delta (float-time (time-since start)))
      (when (> delta t-debug-timer-threshold)
        (with-current-buffer t-buffer-load-times
          (goto-char (point-max))
          (insert (format "package-initialize took %.3f sec\n"
                          delta))))
      res))

  ;; (defadvice require (before t/advice-before-require activate)
  ;;   (message (format "t: %s: Requiring %s"
  ;;                    (current-time-string)
  ;;                    load-file-name)))

  (defadvice require (around t/advice-require activate)
    (let ((start (current-time)) res delta)
      (setq res ad-do-it
            delta (float-time (time-since start)))
      (when (> delta t-debug-timer-threshold)
        (with-current-buffer t-buffer-load-times
          (goto-char (point-max))
          (insert (format "%.3f sec: File %s: Required %s\n"
                          delta load-file-name (ad-get-arg 0)))))
      res))

  (defadvice load (before t/advice-before-load activate)
    (message (format "t: %s: Loading %s"
                     (current-time-string)
                     load-file-name)))

  (defadvice load (around t/advice-load activate)
    (let ((start (current-time)) res delta)
      (setq res ad-do-it
            delta (float-time (time-since start)))
      (with-current-buffer t-buffer-load-times
        (goto-char (point-max))
        (insert (format "%.3f sec: File %s: Loaded %s.\n"
                        delta load-file-name (ad-get-arg 0))))
      res)))

(provide 't-debug)
