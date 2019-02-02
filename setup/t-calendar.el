(t/after calendar
  ;; show week numbers in calendar
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil :height 0.7 :foreground "VioletRed1")
  (copy-face 'default 'calendar-iso-week-header-face)
  (set-face-attribute 'calendar-iso-week-header-face nil :height 0.7 :foreground "VioletRed4")
  (setq calendar-intermonth-text
        '(propertize
          (format "%2d" (car (calendar-iso-from-absolute (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face)
        calendar-intermonth-header (propertize "W " 'font-lock-face 'calendar-iso-week-header-face))
  )
  (provide 't-calendar)