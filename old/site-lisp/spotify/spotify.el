;; TODO this does no longer work after the spotify oauth api

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
          (let ((tracks (assoc 'tracks spotify-results)))
            (cdr (assoc 'items tracks)))))

(defun helm-spotify-play-track (track)
  (spotify-play-track (cdr (assoc 'uri track))))

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
             (format "https://api.spotify.com/v1/search?type=track&q=%s" track))
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
