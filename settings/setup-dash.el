(add-to-list 'load-path "/path/to/dash-at-point")
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)

;; dash search
(global-set-key (kbd "C-c C-j") 'dash-at-point)

(provide 'setup-dash)
