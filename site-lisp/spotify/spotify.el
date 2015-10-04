(require 'url)

(defun spotify-play-track (track)
  (shell-command
   (format "osascript -e 'tell application %S to play track %S'"
           "Spotify"
           track)))

(defun format-track-for-display (track)
  (format "%s: %s - %s"
          (cdr (assoc 'name (elt (cdr (assoc 'artists track)) 0)))
          (cdr (assoc 'name track))
          (cdr (assoc 'name (assoc 'album track)))))

(defun helm-spotify-search ()
  (mapcar (lambda (track)
            (cons (format-track-for-display track) track))
          (cdr (assoc 'tracks spotify-results))))

(defun helm-spotify-play-track (track)
  (spotify-play-track (cdr (assoc 'href track))))

(defvar helm-source-spotify
  '((name . "Spotify")
    (candidates . helm-spotify-search)
    (volatile)
    (action . (("Play" . helm-spotify-play-track)))))

(defun helm-spotify (query)
  (interactive "sSearch spotify for track: ")
  (async-start
   `(lambda ()
      (require 'json)

      (defun spotify-search (track)
        (with-current-buffer
            (url-retrieve-synchronously
             (format "http://ws.spotify.com/search/1/track.json?q=%s" track))
          (goto-char (point-min))
          (re-search-forward "^$")
          (delete-region (+ 1 (point)) (point-min))
          (json-read-object)))

      (spotify-search ,query))
   (lambda (results)
     (setq spotify-results results)
     (helm-update)))

  (helm :sources '(helm-source-spotify)))

(provide 'spotify)
