(require 's)

(defun remark-next-slide ()
  "skip to next slide"
  (interactive)
  (end-of-line)
  (if (search-forward-regexp "---" nil t)
      (move-beginning-of-line 1)
    (end-of-buffer)))

(defun remark-prev-slide ()
  "skip to prev slide"
  (interactive)
  (if (search-backward-regexp "---" nil t)
      (move-beginning-of-line 1)
    (beginning-of-buffer)))

(defun remark-is-at-end-of-buffer ()
  (= (point) (point-max)))

(defun remark-new-separator (sep)
  "adds separator at end of next slide"
  (remark-next-slide)
  (if (remark-is-at-end-of-buffer)
      (insert (concat "\n" sep "\n"))
    (progn
      (insert (concat sep "\n\n"))
      (previous-line))))

(defun remark-new-slide ()
  "creates new slide"
  (interactive)
  (remark-new-separator "---"))

(defun remark-kill-slide ()
  "kill slide"
  (interactive)
  (remark-prev-slide)
  (let ((current-slide-start (point)))
    (next-line)
    (let* ((has-next-slide-marker (search-forward-regexp "---" nil t))
           (next-slide-start (match-beginning 0)))
      (kill-region current-slide-start
                   (if has-next-slide-marker
                       next-slide-start
                     (point-max)))
      (move-beginning-of-line nil))))

(defun remark-create-note ()
  "creates note for slide"
  (interactive)
  (remark-new-separator "???"))

(defun remark-new-incremental-slide ()
  "creates a new incremental slide"
  (interactive)
  (remark-new-separator "--"))

(defun remark-file-string (file-path)
  "get file as string"
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun remark-preview-in-browser ()
  "preview slideshow in browser"
  (interactive)
  (let* ((folder (file-name-directory buffer-file-name))
         (remark-file (concat folder "remark.html"))
         (template-content (remark-file-string remark-file))
         (index-path (concat folder "index.html"))
         (index-content (s-replace "</textarea>"
                                   (concat (buffer-string) "</textarea>")
                                   template-content)))
    (write-region index-content nil index-path nil)))

(defun remark-connect-browser ()
  "serve folder with browsersync"
  (interactive)
  (let ((folder (file-name-directory buffer-file-name)))
    (async-shell-command
     (concat "browser-sync start --files index.html --server "
             (shell-quote-argument folder)
             " --no-open --no-ui --no-online")
     "*remark browser-sync*"
     "*remark browser-sync error*")
    (sit-for 1)
    (message "remark browser-sync connected")
    (shell-command "open http://localhost:3000")))

(defun remark-save ()
  "saves the file and reloads in browser"
  (interactive)
  (save-buffer)
  (if (get-buffer "*remark browser-sync*")
      (remark-preview-in-browser)
    (message
     (concat "Wrote "
             buffer-file-name
             ". Use C-c C-s c to connect to a browser using browser-sync!"))))

(defvar remark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'remark-next-slide)
    (define-key map (kbd "M-p") 'remark-prev-slide)
    (define-key map (kbd "C-x C-s") 'remark-save)
    (define-key map (kbd "C-c C-s s") 'remark-new-slide)
    (define-key map (kbd "C-c C-s i") 'remark-new-incremental-slide)
    (define-key map (kbd "C-c C-s k") 'remark-kill-slide)
    (define-key map (kbd "C-c C-s n") 'remark-create-note)
    (define-key map (kbd "C-c C-s c") 'remark-connect-browser)
    map)
  "keymap for `remark-mode'")

(defvar remark-mode-syntax-table
  (let ((st (make-syntax-table))) st)
  "syntax table for `remark-mode'")

(defconst remark-font-lock-defaults
  (list
   (cons "---" font-lock-warning-face)
   (cons "\\?\\?\\?" font-lock-comment-face)
   (cons "\\(template\\|name\\|class\\)" font-lock-comment-face))
  "keyword highlight for `remark-mode'")

(add-to-list 'auto-mode-alist '("\\.remark\\'" . remark-mode))

(define-derived-mode
  remark-mode
  markdown-mode
  "remark"
  "a major mode for editing remark files"
  :syntax-table remark-mode-syntax-table
  (setq-local font-lock-defaults (list (append remark-font-lock-defaults markdown-mode-font-lock-keywords))))

(provide 'remark-mode)
