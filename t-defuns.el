;;; t-defuns.el -*- lexical-binding: t; -*-
;;; t-defuns.el --- more or less useful functions
;;; Commentary:
;;; Code:

(defun t/exec-org-block (name vars)
  "Excecute `NAME'd org block."
  (save-excursion
    (goto-char (org-babel-find-named-block name))
    (org-babel-execute-src-block nil
                                 (org-babel-lob-get-info)
                                 (seq-map (lambda (var)
                                            (cons :var var))
                                          vars))))

(defun t/shell-command-to-string (cmd)
  "Run `CMD' as shell command, trim trailing newlines."
  (t/trim-final-newline (shell-command-to-string cmd)))

(defun t/async-shell-command (name cmd &optional fn)
  "Execute `CMD' async, call `FN' with the result string."
  (let*
      ((fn fn)
       (buf-name (format "*%s*" name))
       (_ (when (get-buffer-create buf-name)
            (with-current-buffer buf-name (erase-buffer))))
       (p (start-process-shell-command name buf-name cmd)))
    (prog1 p
      (when fn
        (set-process-sentinel p (lambda (process event)
                                  (with-current-buffer buf-name
                                    (let ((res (buffer-substring-no-properties (point-min) (point-max))))
                                      (apply fn (list process
                                                      event
                                                      (split-string res "\r?\n")))))))))))

(defun t/evil-ex-define-cmd-local (cmd function)
  "Locally binds the function FUNCTION to the command CMD."
  (unless (local-variable-p 'evil-ex-commands)
    (setq-local evil-ex-commands (copy-alist evil-ex-commands)))
  (evil-ex-define-cmd cmd function))

(defun t/css-kill-value ()
  "kills the attribute of a css property."
  (interactive)
  (let ((pos (point)))
    (move-beginning-of-line 1)
    (if (search-forward-regexp ": ?")
        (progn
          (when (not (looking-at-p ";"))
            (kill-word 1)
            (just-one-space)))
      (move-to-column pos))))

(defmacro t/rename-modeline (package-name mode new-name)
  "per package modeline rename for mode."
  `(eval-after-load ,package-name
    '(defadvice ,mode (after t/rename-modeline activate)
       (setq mode-name ,new-name))))

(defun t/json-format ()
  "pretty prints json in selected region."
  (interactive)
  (save-excursion
    (if (featurep 'apheleia)
        (apheleia-format-buffer 'prettier-json)
      (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t))))

(defun t/build-tags ()
  "Build ctags file for projectile project, call load-tags when done.

Effectively runs this for the current git project, e.g:

ctags -e -R --options=~/.emacs.d/ctags -f ~/.emacs.d/TAGS ~/.emacs.d/

The same can be done for the current folder only to place a TAGS file in it:

ctags -e -R .

Remember to build Emacs --without-ctags and use the one from `brew' instead,
it's the one with the correct options needed to generate ctags that Emacs
understands."
  (interactive)
  (message "building project tags..")
  (lexical-let* ; so lambdas create closures
   (;; (ctags (expand-file-name "~/.emacs.d/ctags"))
    (root (projectile-project-root))
    (tags (shell-quote-argument (concat root "TAGS")))
    (process (start-process-shell-command "build ctags asynchronously"
                                          "*ctags async*"
                                          (concat
                                           "ctags -e -R"          ; recurse
                                           " --options=" ctags ; use global config
                                           " -f " tags " "     ; put it in project/TAGS
                                           " ."                   ; in the current directory
                                           ))))
   (set-process-sentinel process (lambda (process event)
                                   (t/load-tags tags)))))

(defun t/load-tags (tags)
  "Loads project tags into tag table."
  (message "loading project tags..")
  (visit-tags-table tags)
  (message "project tags loaded"))

(defun t/find-tag-at-point ()
  "Go to tag at point, builds and/or load project TAGS file first."
  (interactive)
  (let* ((root (projectile-project-root))
         (tags (shell-quote-argument (concat root "TAGS"))))
    (if (file-exists-p tags) (t/load-tags tags) (t/build-tags))
    (when (find-tag-default)
      (etags-select-find-tag-at-point))))

(defun t/copy-to-clipboard (text &optional push)
  "Copy text to os clipboard. Cygwin uses cygutils-extra's `putclip`. Mac uses builtin pbcopy."
  (let* ((process-connection-type nil)
         (copy-cmd (if is-mac "pbcopy" "putclip"))
         (proc (start-process copy-cmd "*Messages*" copy-cmd)))
    (process-send-string proc text)
    (process-send-eof proc))
  text)

(defun t/open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-06-12"
  (interactive)
  (cond
   (is-win (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   (is-cygwin (shell-command (concat "cygstart " (shell-quote-argument default-directory))))
   (is-mac (shell-command "open ."))
   (is-linux (shell-command "thunar ."))))

(defun t/hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

(defun t/hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

(defun t/untabify-buffer ()
  "Remove tabs in buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun t/indent-buffer ()
  "Correctly indent a buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun t/cleanup-buffer-whitespace-and-indent ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (t/untabify-buffer)
  (when (fboundp 'whitespace-cleanup) (whitespace-cleanup))
  (t/indent-buffer))

(defun t/eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(defun t/eval-and-replace ()
  "Evaluate and replace the preceding sexp with its value."
  (interactive)
  (if (eq major-mode 'clojure-mode)
      (cider-eval-last-sexp-and-replace)
    (progn
      (backward-kill-sexp)
      (condition-case nil
          (prin1 (eval (read (current-kill 0)))
                 (current-buffer))
        (error (message "Invalid expression")
               (insert (current-kill 0)))))))

;; run it with M-x lorem
(define-skeleton lorem
  "Insert a lorem ipsum."
  nil
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(defun t/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun t/delete-frame-or-hide-last-remaining-frame ()
  "Delete the selected frame. If the last one, hide it instead."
  (interactive)
  (condition-case nil
      (delete-frame)
    (error (if (window-system)
               (when is-mac (ns-do-hide-emacs))
             "in terminal, use c-z instead"))))

(defun t/delete-window-or-frame-or-hide ()
  "Delete selected window or frame.
If its the last window in the frame, delete the frame. If its the last frame,
hide it (on mac)."
  (interactive)
  (condition-case err
      (delete-window)
    (error
     (if (length= (visible-frame-list) 1)
         (if is-mac
             (ns-do-hide-emacs)
           ;; last frame on linux
           (error (save-buffers-kill-emacs)))
       (condition-case errr
           (delete-frame)
         (error (save-buffers-kill-emacs)))))))

(defun t/copy-buffer-file-name ()
  "Copy the current buffer file name into the kill ring."
  (interactive)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s'" filename))))

(defun t/copy-buffer-file-path ()
  "Copy the full path to the current file into the kill ring."
  (interactive)
  (let ((filename (file-name-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file path '%s'" filename))))

(defun t/previous-window ()
  "Skip back to previous window."
  (interactive)
  (call-interactively 'other-window))

(defun t/buffer-mode (&optional buffer-or-string)
  "Returns the major mode associated with a buffer."
  (interactive)
  (let ((mode (with-current-buffer (or buffer-or-string (current-buffer))
                major-mode)))
    (message "major mode: %s" mode)
    mode))

(defun t/split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun t/split-window-below-and-move-there-dammit ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun t/face-color-b (&optional face)
  "Get `:background' color of `FACE'."
  (interactive)
  (face-attribute (or face (face-at-point)) :background))

(defun t/face-color-f (&optional face)
  "Get `:foreground' color of `FACE'."
  (interactive)
  (face-attribute (or face (face-at-point)) :foreground))

(defun server-remove-kill-buffer-hook ()
  ;; remove other clients-has-the-file-open-prompt
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(defun t/prefix-arg-universal? ()
  "Check if the function was called with the c-u universal prefix."
  (equal '(4) current-prefix-arg))

(defun t/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun t/uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun t/sort-lines ()
  "Sort lines in region or current buffer."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun t/find-xml-path ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun t/date-time ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00"
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun t/date-time-for-filename ()
  "Return date-time iso 8601 string suitable for filename."
  (replace-regexp-in-string ":" "." (t/date-time)))

(defun t/find-org-files-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
         (filext (or filext "org$\\\|org_archive$\\\|\\.txt$"))
         (fileregex (format "%s\\.\\(%s\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (push file-or-dir org-file-list)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (t/find-org-files-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (push org-file org-file-list)))))))

(defun t/org-fix-inline-images ()
  "Fix redisplaying images after executing org babel code."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (eq 'org-mode major-mode)
        (org-redisplay-inline-images)))))

(defun t/project-root ()
  "Get project root without throwing."
  (interactive)
  (let (projectile-require-project-root strict-p)
    (or (projectile-project-root)
        default-directory)))

(defun t/volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (setq-local kill-buffer-query-functions
              (remq 'process-kill-buffer-query-function
                    kill-buffer-query-functions))
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun t/volatile-kill-buffer-and-window ()
  "Kill current buffer and the window unconditionally."
  (interactive)
  (if (and
       (= (pos-bol) (pos-eol) )
       (= (save-excursion (goto-char (point-max)))
          (point)))
      (progn
        (setq-local kill-buffer-query-functions
                    (remq 'process-kill-buffer-query-function
                          kill-buffer-query-functions))
        (set-buffer-modified-p nil)
        (kill-buffer-and-window))
    (when (and
           (not (called-interactively-p))
           (eq evil-state 'insert)) (delete-char 1))))

(defun t/remove-newlines (str)
  (replace-regexp-in-string "[\r\n]+" "" str))

(defun t/grab-url ()
  (interactive)
  (let* ((browser 'ff)
         (fn (if (called-interactively-p) 'message 'identity))
         (url (cond
               ((eq browser 'ff) (t/grab-firefox-url))
               ((eq browser 'chrome) (t/grab-chrome-url)))))
    (funcall fn url)))

(defun t/insert-url ()
  (interactive)
  (insert (t/grab-url)))

(defun t/grab-chrome-url ()
  "Grab the frontmost url out of chrome using `org-mac-grab-link'."
  (interactive)
  (t/remove-newlines
   (shell-command-to-string
    "osascript -l JavaScript -e 'Application(\"Google Chrome\").windows.at(0).activeTab.url()'")))

(defun t/grab-firefox-url (&optional browser)
  "Grab the frontmost url of firefox using `t/osascript-activate'. Does not work
with browser in full screen."
  (interactive)
  (let ((browser (or browser "Firefox Developer Edition")))
    (t/osascript-activate browser)
    (t/async-shell-command "wait-bring-back-emacs" "sleep 0.2 && osascript -e 'tell application \"Emacs\" to activate'")
    (sit-for 0.2)
    (t/remove-newlines
     (t/run-osascript
      (concat
       "tell application \"System Events\" to get value of UI element 1 of combo box 1 of toolbar \"Navigation\" of first group of front window of application process \"" browser "\"")))))

(defun t/browse-firefox-url-in-eww ()
  "Open the frontmost firefox url in `eww'."
  (interactive)
  (eww (t/grab-firefox-url)))

(defun t/browse-chrome-url-in-eww ()
  "Open the frontmost chrome url in `eww'."
  (interactive)
  (eww (t/grab-chrome-url)))

(defun t/get-url (url)
  "Get URL synchronously."
  (with-current-buffer (url-retrieve-synchronously url)
    (buffer-string)))

(defun t/fetch (url)
  "Insert URL contents in current buffer.
  Drops headers and 2x empty lines before content."
  (interactive "sfetch url:")
  (insert (t/get-url url))
  (goto-char (point-min))
  (kill-sentence)
  (kill-line)
  (kill-line))

(defun t/fetch-firefox-url ()
  "Insert contents of frontmost url of firefox in buffer."
  (interactive)
  (t/fetch (t/grab-firefox-url)))

(defun t/fetch-chrome-url ()
  "Insert contents of frontmost url of chrome in buffer."
  (interactive)
  (t/fetch (t/grab-chrome-url)))

(defun t/browse-url-at-point ()
  "Open url at `point' from everywhere.
Prefix arg will force eww."
  (interactive)
  (let ((browse-url-browser-function
         (if (t/prefix-arg-universal?)
             'eww-browse-url
           'browse-url-default-browser)))
    (cond ((equal major-mode 'eww-mode) (eww-browse-with-external-browser))
          ((equal major-mode 'elfeed-search-mode) (elfeed-search-browse-url))
          ((equal major-mode 'elfeed-show-mode) (browse-url (elfeed-entry-link elfeed-show-entry)))
          ((equal major-mode 'gnus-summary-mode) (gnus-summary-browse-url))
          ((equal major-mode 'mu4e-headers-mode) (browse-url (shr-url-at-point nil)))
          ((equal major-mode 'mu4e-view-mode) (browse-url (shr-url-at-point nil)))
          ((equal major-mode 'magit-status-mode) (call-interactively 'forge-browse-dwim))
          ((equal major-mode 'hackernews-mode) (hackernews-browse-url-action (button-at (point))))
          ((equal major-mode 'org-mode) (org-open-at-point))
          (t (call-interactively 'browse-url-at-point)))))

(defun t/open-elfeed-in-eww ()
  (interactive)
  (eww (elfeed-entry-link elfeed-show-entry)))

(defun t/last-weekday-of-month-p ()
  (let* ((day-of-week (calendar-day-of-week date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-month-day (calendar-last-day-of-month month year))
         (month-day (cadr date)))

    (or
     ;; it's the last day of the month & it is a weekday
     (and (eq month-day last-month-day)
          (memq day-of-week '(1 2 3 4 5)))

     ;; it's a friday, and it's the last-but-one or last-but-two day
     ;; of the month
     (and (eq day-of-week 5)
          (or (eq month-day (1- last-month-day))
              (eq month-day (1- (1- last-month-day))))))))

(defun t/face-at-point (pos)
  "Echo the face at POS point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun t/font-lock-test-faces ()
  "Outputs test strings with all font lock faces to show colors."
  (interactive)
  (let ((b (get-buffer-create "*t/font-lock-test-faces*")))
    (with-current-buffer b
      (dolist (face '(font-lock-warning-face
                      font-lock-function-name-face
                      font-lock-variable-name-face
                      font-lock-keyword-face
                      font-lock-comment-face
                      font-lock-comment-delimiter-face
                      font-lock-type-face
                      font-lock-constant-face
                      font-lock-builtin-face
                      font-lock-preprocessor-face
                      font-lock-string-face
                      font-lock-doc-face
                      font-lock-negation-char-face))
        (let ((current-string (concat "\n" (symbol-name face)))
              (is-font-lock font-lock-mode))
          (when is-font-lock (font-lock-mode 0))
          (put-text-property 1 (length current-string) 'face face current-string)
          (insert current-string)
          (when is-font-lock (font-lock-mode 1))) t))
    (popwin:display-buffer b t)))

(defun t/run-osascript (s &optional discard-output)
  "Run applescript."
  (let ((b-stderr "*osascript-err*")
        (b-stdout "*osascript*"))
    (t/kill-buffer b-stderr)
    (t/kill-buffer b-stdout)
    (with-current-buffer (get-buffer-create b-stdout)
      (shell-command (format "/usr/bin/osascript -e '%s'" s) b-stdout (get-buffer-create b-stderr))
      (buffer-string))))

(defun t/kill-buffer (b)
  (when (get-buffer b) (kill-buffer b)))

(defun t/osascript-activate (app)
  "Run applescript to activate application."
  (t/run-osascript (format "tell application \"%s\" to activate" app) t))

(defun t/osascript-show-url (url)
  (t/run-osascript
   (format "tell application \"Google Chrome\" to set URL of active tab of window 1 to \"%s\"" url)))

(defun t/open-in-intellij ()
  "Opens current file in IntelliJ IDEA."
  (interactive)
  (async-shell-command
   (let* ((cmd "/Applications/IntelliJ\\ IDEA.app/Contents/MacOS/idea %s")
          (args " --line %d --column %d %s")
          (root (t/project-root))
          (file-name (buffer-file-name)))
     (if file-name
         (format (concat cmd args)
                 root
                 (line-number-at-pos)
                 (current-column)
                 (shell-quote-argument file-name))
       (format cmd root))))
  (t/osascript-activate "IntelliJ IDEA"))

(defun t/propertize-read-only (str)
  (propertize str
              'read-only t
              'front-sticky 'read-only
              'rear-nonsticky 'read-only))

(defun t/strip-text-properties (txt)
  (with-temp-buffer
    (insert txt)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun t/get-file-string (file-path)
  "Return `FILE-PATH's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun t/re-seq (regexp string)
  "Get a list of all REGEXP matches (from the first group) in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun t/re-seq-in-file (regex file)
  (t/re-seq regex (t/get-file-string (t/user-emacs-file file))))

(defun t/buffer-finished-p (b)
  "Return non-nil value if the buffer B has an ended process."
  (string-match-p
   "^Process .* \\(finished\\|exited\\).*$"
   (car (last
         (split-string
          (with-current-buffer b
            (let ((len (length (buffer-string))))
              (if (> len 0)
                  (buffer-substring-no-properties 1 len)
                "")))
          "\n")))))

(defun t/term-quit-if-finished (&optional else)
  (interactive)
  (if (t/buffer-finished-p (current-buffer))
      (t/volatile-kill-buffer)
    (when (functionp else)
      (call-interactively else))))

(defun t/term-kill-if-finished (&optional else)
  (interactive)
  (if (t/buffer-finished-p (current-buffer))
      (t/volatile-kill-buffer-and-window)
    (when (functionp else)
      (call-interactively else))))

(defun t/newline-expand-braces ()
  "Newline like `evil-ret', but expand (), [] and {} with newline in between,
  and indent accordingly."
  (interactive)
  (cond
   ((or (and (looking-back "(") (looking-at ")"))
        (and (looking-back "\\[") (looking-at "\\]"))
        (and (looking-back "{") (looking-at "}")))
    (progn
      (save-excursion (indent-according-to-mode))
      (evil-ret)
      (save-excursion (indent-according-to-mode))
      (t/open-line-above)))
   (t (progn
        (evil-ret)
        (indent-according-to-mode)))))

(defun t/unbind (fn-or-s)
  "Unbind function or symbol depending on type."
  (interactive)
  (cond ((fboundp fn-or-s) (fmakunbound fn-or-s))
        ((symbolp fn-or-s) (makunbound fn-or-s))
        (t (message "Can't unbind %s, not a fboundp or symbolp" fn-or-s))))

(defun t/add-to-list (l item-or-items)
  "Adds items to the list `L'."
  (if (listp item-or-items)
      (dolist (item item-or-items) (add-to-list l item t))
    (add-to-list l item-or-items))
  l)

(defun t/highlight-logging ()
  "Add log highlighting to current major mode."
  (interactive)
  (defvar t-log-debug-face 'doom-modeline-buffer-path)
  (defvar t-log-info-face 'doom-modeline-info)
  (defvar t-log-warn-face 'doom-modeline-warning)
  (defvar t-log-error-face 'doom-modeline-urgent)
  (defvar t-log-comment-face 'doom-modeline-eldoc-bar)
  (defvar t-log-keyword-face 'font-lock-keyword-face)
  (defvar t-log-hl-face 'org-code)
  (font-lock-add-keywords
   nil
   `(
     (,(rx (and "[" (group (one-or-more (or alnum "-"))) "]"))       1 t-log-keyword-face t) ; [module]
     (,(rx (group (or "debug" "DEBUG")))                             1 t-log-debug-face t) ; [DEBUG]
     (,(rx (group (or "info" "INFO" "SUCCESS")))                     1 t-log-info-face t) ; [INFO]
     (,(rx (group (or "warning" "warn" "WARNING" "WARN" "SKIPPED"))) 1 t-log-warn-face t) ; [WARN] [WARNING]
     (,(rx (group (or "error" "ERROR" "FAILURE" "Caused by:")))      1 t-log-error-face t) ; [ERROR]

     ;; 2017-07-11T14:45:11.067+02
     ;; 2017-07-11T14:45:11.067+02:00
     (,(rx (group (repeat 4 digit) "-" (repeat 2 digit) "-" (repeat 2 digit)
                  (or " " "T")
                  (repeat 2 digit) ":" (repeat 2 digit) ":" (repeat 2 digit)
                  (optional (or "," ".") (repeat 3 digit))
                  (optional (or "Z"
                                (and "+"
                                     (repeat 2 digit)
                                     (optional ":" (repeat 2 digit))))))) 1 t-log-hl-face t)
     ;; 14:15
     ;; 14:15:11
     ;; 14:15.11
     (,(rx space
           (group (repeat 2 digit)
                  ":" (repeat 2 digit)
                  (optional (or ":" ".") (repeat 2 digit)))) 1 t-log-hl-face t)
     ;; :modules
     ;; :some:shadowJar
     ;; :some:modu-le:shadowJar
     (,(rx (group (>= 0 (and ":" (one-or-more (or alpha "-")))))) 1 t-log-keyword-face t)

     ;; com.some.domain
     (,(rx (group (one-or-more letter)
                  (>= 2 (and "." (one-or-more (or digit letter "$"))))
                  (optional "@" (one-or-more alnum)))) 1 t-log-comment-face t)

     ;; Tests run: 2, Failures: 0, Errors: 0, Skipped: 0
     (,(rx (and (zero-or-more (any ".")) "Tests run: " (group digit (zero-or-more digit)) (zero-or-more (any ".")))) 1 t-log-info-face t)
     (,(rx (and (zero-or-more (any ".")) "Failures: "  (group digit (zero-or-more digit)) (zero-or-more (any ".")))) 1 t-log-warn-face t)
     (,(rx (and (zero-or-more (any ".")) "Errors: "    (group digit (zero-or-more digit)) (zero-or-more (any ".")))) 1 t-log-error-face t)
     (,(rx (and (zero-or-more (any ".")) "Skipped: "   (group digit (zero-or-more digit)) (zero-or-more (any ".")))) 1 t-log-warn-face t)

     ;; 0.662 s, 2s, 30m, 10 min, 4h, 20 minutes, 1hour, 20 hours
     (,(rx (group (one-or-more digit)
                  (optional (or "," "." ":") (one-or-more digit))
                  (optional " ")
                  (or (or "ms" "millisecond" "milliseconds")
                      (or "s" "sec" "second" "seconds")
                      (or "m" "min" "minute" "minutes")
                      (or "h" "hour" "hours")))
           word-boundary) 1 t-log-keyword-face t)

     ;; 1.2, 2,3, 3,33, 4.8, 4.2
     (,(rx space
           (group (one-or-more digit)
                  (or ?, ?.)
                  (one-or-more digit))
           word-boundary) 1 t-log-keyword-face t)

     ;; 1G, 2M, 3K, 4B, 5% wow
     ;; 1.2G, 2,3M, 3,33K, 4.8B, 4.2%, 1G wow
     (,(rx (group (one-or-more digit)
                  (optional (or ?, ?.) (one-or-more digit))
                  (in ?% ?G ?M ?K ?B))
           word-boundary) 1 t-log-info-face t)))

  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer)))))

(defun t/clone (repo)
  "Clone a github REPO to `~/Projects/<repo-name>'."
  (interactive "sClone repository: ")
  (require 'magit)
  (when-let* ((repo-name (or (and (s-starts-with? "https://github." repo)
                                  (replace-regexp-in-string "https://github\\.\\(?:com\\|dev\\)/\\([^/]+\\)/\\([^/]+\\)/?.*?$" "\\1/\\2" repo))
                             (and (s-ends-with? ".git" repo)
                                  (replace-regexp-in-string "git@github.com:\\(.+\\).git" "\\1" repo)))))
    (let*
        ((dir (expand-file-name (format "~/Projects/%s/" repo-name)))
         (cmd (concat "git " "clone " (format "git@github.com:%s.git" repo-name)))
         (msg (concat "Cloning " repo-name ".. ok.")))
      (if (file-exists-p dir)
          (dired dir)
        (progn
          (message "Cloning %s.." repo repo-name)
          (t/shell-command-to-string (concat "mkdir -p" " " dir))
          (t/async-shell-command (format "*git-clone %s*" repo-name)
                                 (concat cmd " " (magit-convert-filename-for-git dir))
                                 (lambda (&optional &rest args)
                                   (dired dir)
                                   (message msg))))))))

(defun t/visit-git-link-pulls ()
  "Navigate to /pulls for the current git repo."
  (interactive)
  (let* ((origin (magit-get
                  "remote"
                  (or (magit-get-remote "main")
                      (magit-get-remote "master")) "url"))
         (url (replace-regexp-in-string
               ".+\\.com:\\(.+\\)\\.git"
               "\\1"
               origin)))
    (browse-url
     (format "%s/pulls"
             (plist-get (browse-at-remote--get-url-from-remote url) :url)))))

(defun t/projectile-dired ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "dired in project: "
         projects
         :action 'dired)
      (user-error "No projects found"))))

(defun t/projectile-magit-status ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    ;; TODO
    ;; (autoload-do-load 'my-function 'my-file nil t)
    (if projects
        (projectile-completing-read
         "magit status in project: "
         projects
         :action (lambda (project)
                   (let ((default-directory project))
                     (magit-status project))))
      (user-error "No projects found"))))

(defun t/projectile-rg ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "rg in project: "
         projects
         :action (lambda (project)
                   (let ((default-directory project))
                     (cond
                      ((fboundp '+default/search-project) (+default/search-project))
                      ((fboundp 'counsel-projectile-rg) (counsel-projectile-rg))))))
      (user-error "No projects found"))))

(defun t/projectile-visit-git-link-pulls ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Visit pulls for project: "
         projects
         :action (lambda (project)
                   (let* ((default-directory (expand-file-name project)))
                     (t/visit-git-link-pulls))))
      (user-error "No projects found"))))

(defun t/visit-git-link-pulls ()
  "Navigate to /pulls for the current git repo."
  (interactive)
  (let* ((origin (magit-get
                  "remote"
                  (or (magit-get-remote "main")
                      (magit-get-remote "master")) "url"))
         (url (replace-regexp-in-string
               ".+\\.com:\\(.+\\)\\.git"
               "\\1"
               origin)))
    (browse-url
     (format "https://www.github.com/%s/pulls" url))))

(defun t/in-all-windows (fn)
  (interactive)
  (dolist (w (window-list))
    (funcall fn w)))

(defun t/in-all-buffers (fn)
  (interactive)
  (dolist (b (doom-buffer-list))
    (with-current-buffer b
      (funcall fn b))))

(defun t/set-all-window-margins (l r)
  (dolist (w (window-list))
    (set-window-margins w l r)))

(defvar *t-window-margins* 0)

(defun t/reset-window-margins (&optional m)
  "Reset window margins"
  (interactive)
  (when m
    (setq *t-window-margins* m))
  (t/set-all-window-margins *t-window-margins*
                            *t-window-margins*))

(defun t/margins-global (&optional m)
  "Set global window margins M."
  (interactive (list (string-to-number (read-string "Width: " nil nil "0"))))
  (let ((m (or m 0)))
    (setq-default left-margin-width m
                  right-margin-width m)
    (t/reset-window-margins m)
    (add-hook 'window-setup-hook 't/reset-window-margins)
    (add-hook 'window-configuration-change-hook 't/reset-window-margins)
    (add-hook '+popup-buffer-mode-hook 't/reset-window-margins)))

(defun t/margins-global-decrease ()
  "Decrease global window margins."
  (interactive)
  (let ((w (- left-margin-width 1)))
    (when (> w 0)
      (t/margins-global w))))

(defun t/margins-global-increase ()
  "Increase global frame margins."
  (interactive)
  (let ((w (+ left-margin-width 1)))
    (when (> w 0)
      (t/margins-global w))))

(defun t/margins-local (l)
  "Set buffer local window margin."
  (interactive (list (string-to-number (read-string "Width: " nil nil "0"))))
  (let ((l (or l 0)))
    (setq-default left-margin-width l
                  right-margin-width l)
    (set-window-margins (get-buffer-window (current-buffer)) l l)))

(defun t/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (eww-toggle-images)
  (message "Images are now %s" (if shr-inhibit-images "off" "on")))

(defun t/last-used-window-buffer ()
  "Switch to the window that displays the most recently selected buffer."
  (interactive)
  (let* ((buffers (delq (current-buffer) (buffer-list (selected-frame))))
         (windows (delq (selected-window) (delq nil (mapcar #'get-buffer-window buffers)))))
    (if windows
        (select-window (car windows))
      (message "no suitable window to switch to"))))

(defun t/word-at-point ()
  "Return word under cursor."
  (thing-at-point 'word t))

(defun t/ip ()
  (interactive)
  (let ((ip (-> "dig +short myip.opendns.com @resolver1.opendns.com" shell-command-to-string s-trim)))
    (message ip)
    (kill-new ip)))

(defun t/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive (list
                (string-to-number
                 (read-string (format "Opacity is %s. Enter a value from 0 - 100, or press enter for 100: "
                                      (frame-parameter (selected-frame) 'alpha))
                              nil
                              nil
                              "100"))))
  (set-frame-parameter (selected-frame) 'alpha value))

(defun t/clone-github-repo-from-chrome ()
  "Clones the github repo of the currently visisble chrome tab."
  (interactive)
  (t/clone (t/grab-chrome-url)))

(defun t/ediff-use-both ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun t/lookup-key (key)
  "Search for KEY in all known keymaps."
  (mapatoms
   (lambda (ob)
     (when (and (boundp ob) (keymapp (symbol-value ob)))
       (when (functionp (lookup-key (symbol-value ob) key))
         (message "%S" ob))))
   obarray))

(defun t/lookup-key-prefix (key)
  "Search for KEY as prefix in all known keymaps."
  (mapatoms
   (lambda (ob)
     (when (and (boundp ob) (keymapp (symbol-value ob)))
       (when (let ((m (lookup-key (symbol-value ob) key)))
               (and m (or (symbolp m) (keymapp m))))
         (message "%S" ob))))
   obarray))

(defun t/fns ()
  "List functions."
  (let ((l))
    (mapatoms (lambda (a)
                (when (functionp a)
                  (push a l))))
    l))

(defun t/interactive-fns ()
  "List interactive functions."
  (-filter 'commandp
           (t/fns)))

(defun t/locally-disable-cursor ()
  "Locally disable cursor in buffer."
  (interactive)
  (make-local-variable 'cursor-type)
  (setq-local cursor-type 'nil)
  (setq-local evil-normal-state-cursor 'nil))

(defun t/locally-enable-cursor ()
  "Enable locally disabled cursor in buffer."
  (interactive)
  (kill-local-variable cursor-type)
  (setq-local evil-normal-state-cursor 'box))

(defun t/random-line ()
  "Goto a random line in the buffer. Useful trying out a random package."
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (forward-line (random (count-lines (point-min) (point-max)))))

(defun t/frame-width ()
  (interactive)
  (let ((w (frame-width (selected-frame))))
    (message "width: %s" w)
    w))

(defun t/frame-height ()
  (interactive)
  (let ((h (frame-height (selected-frame))))
    (message "height: %s" h)
    h))

(defun t/decrease-frame-width ()
  "Decrease Emacs frame size horizontally."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-width frame (- (frame-width frame) 10))))

(defun t/increase-frame-width ()
  "Increase Emacs frame size horizontally."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-width frame (+ (frame-width frame) 10))))

(defun t/decrease-frame-height ()
  "Decrease Emacs frame size vertically."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-height frame (- (frame-height frame) 10))))

(defun t/increase-frame-height ()
  "Increase Emacs frame size vertically."
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-height frame (+ (frame-height frame) 10))))

(defun t/move-frame-right ()
  "Move Emacs frame right."
  (interactive)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left)))
    (set-frame-parameter frame 'left (+ left 40))))

(defun t/move-frame-left ()
  "Move Emacs frame left."
  (interactive)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left)))
    (set-frame-parameter frame 'left (- left 40))))

(defun t/move-frame-up ()
  "Move Emacs frame up."
  (interactive)
  (let* ((frame (selected-frame))
         (top (frame-parameter frame 'top)))
    (set-frame-parameter frame 'top (- top 40))))

(defun t/move-frame-down ()
  "Move Emacs frame down."
  (interactive)
  (let* ((frame (selected-frame))
         (top (frame-parameter frame 'top)))
    (set-frame-parameter frame 'top (+ top 40))))

(defun t/diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month.

  ORG-MODE:  * My Task
  SCHEDULED: <%%(diary-last-day-of-month date)>
  DIARY:  %%(diary-last-day-of-month date) Last Day of the Month

  See also:  (setq org-agenda-include-diary t)
  \(diary-last-day-of-month '(2 28 2017))"

  (require 'org-clock)
  (= (calendar-extract-day date)
     (calendar-last-day-of-month (calendar-extract-month date)
                                 (calendar-extract-year date))))

(defun t/diary-last-day-of-week ()
  "Return `t` if DATE is the last day of the week."
  (require 'org-clock)
  (= 5 ; friday
     (org-day-of-week (calendar-extract-day date)
                      (calendar-extract-month date)
                      (calendar-extract-year date))))

(defun t/popwin-next-key (key)
  (interactive "kPress keybinding to run in popup: ")
  (popwin:display-buffer-1 (popwin:dummy-buffer))
  (let ((keys (key-binding key)))
    (funcall
     (cond ((stringp keys) (symbol-function keys))
           (t keys)))))

(defun t/popwin (fn)
  "Run function FN in popwin."
  (t/lambda ()
            (popwin:display-buffer-1 (popwin:dummy-buffer))
            (funcall fn)))

(defun t/search-cheat-sh (&optional input)
  "Search `http://cheat.sh/' for help on commands and code."
  (interactive)
  ;; (interactive "sCommand or Topic: ")
  (message "this does not work any longer

  curl http://cheat.sh/:list lists all commands

  (eww (concat  http://cheat.sh/  input)) can show them
  ?T&q params give text only, no twitter buttons
  "))

(defun t/toggle-dedicated-window (&optional &rest params)
  (interactive)
  (let ((dedicated (if (> (length params) 0)
                       (car params)
                     (not (window-dedicated-p (selected-window))))))
    (set-window-dedicated-p (selected-window) dedicated)
    (set-window-parameter (selected-window) 'no-delete-other-windows dedicated)
    (message (if dedicated "Window dedicated." "Window not dedicated."))))

(defun t/compile (dir)
  (interactive "DCompile in directory: ")
  (let ((default-directory (expand-file-name dir)))
    (compile "make")))

(defun t/gradle-build ()
  (interactive)
  (let ((default-directory (t/project-root)))
    (compile (if (t/prefix-arg-universal?)
                 "gw clean build"
               "gw build"))))

(defun t/gradle-detekt-auto-correct ()
  (interactive)
  (let ((default-directory (t/project-root)))
    (compile "gw detekt --auto-correct")))

(defun t/where-am-i ()
  (interactive)
  (require 'xref)
  (let ((l (xref-location-marker
            (xref-make-file-location
             (buffer-file-name)
             (line-number-at-pos)
             (current-column)))))
    (xref--show-pos-in-buf l (marker-buffer l))))

(defun t/format-xml ()
  "Reformat and replace buffer content using xmllint. Install on macos using `brew install libxml2`."
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "xmllint --format -"
   (current-buffer) t))

(defun t/trim-final-newline (string)
  (let ((len (length string)))
    (cond
     ((and (> len 0) (eql (aref string (- len 1)) ?\n))
      (substring string 0 (- len 1)))
     (t string))))

(defun t/read-file (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (t/trim-final-newline (buffer-string))))

(defun t/clear-kill-ring ()
  "Clear the kill ring."
  (setq kill-ring nil))

(defun t/mark-real-buffer ()
  (interactive)
  (doom-set-buffer-real (current-buffer) t))

(defun t/mark-unreal-buffer ()
  (interactive)
  (doom-set-buffer-real (current-buffer) nil))

(defun t/dired-subtree-toggle ()
  (if (eq major-mode 'dired-sidebar-mode)
      (dired-sidebar-subtree-toggle)
    (dired-subtree-toggle)))

(defun t/dired-close-recursively ()
  "Close all directories starting with this directory's path."
  (interactive)
  (let ((path (dired-get-filename)))
    (save-excursion
      (while
          (and
           (not (eq (point) (save-excursion (end-of-buffer) (point))))
           (s-starts-with? path (dired-get-filename)))
        (when (dired-subtree--is-expanded-p)
          (t/dired-subtree-toggle))
        (forward-line)))))

(defun t/dired-open-recursively ()
  "Open all directories starting with this directory's path."
  (interactive)
  (let ((path (dired-get-filename)))
    (save-excursion
      (when (dired-subtree--is-expanded-p) (t/dired-subtree-toggle))
      (t/dired-subtree-toggle)
      (forward-line)
      (t/dired-show-recursively-0 path))))

(defun t/dired-ignored? ()
  "File under cursor is ignored by projectile. Only checks file-name-base."
  (interactive)
  (-any?
   (lambda (d) (s-matches? d (file-name-base (dired-get-filename nil t))))
   projectile-globally-ignored-directories))

(defun t/dired-show-recursively-0 (path)
  "Recursively opens all directories for this path."
  (interactive)
  (when (not (eq (point) (save-excursion (end-of-buffer) (point))))
    (when (and
           (not (t/dired-ignored?))
           (s-starts-with? path (dired-get-filename nil t))
           (not (s-starts-with? "." (file-name-base (dired-get-filename nil t))))
           )
      (when (dired-subtree--is-expanded-p) (t/dired-subtree-toggle))
      (t/dired-subtree-toggle))
    (forward-line)
    (t/dired-show-recursively-0 path)))

(defun t/dired-locate-path (path)
  "Locate PATH in a dired listing."
  (beginning-of-buffer)
  (forward-line)
  (dolist (el path)
    (search-forward el nil t)
    (when (not (dired-subtree--is-expanded-p))
      (dired-subtree-toggle)))
  (dired-move-to-filename))

(defun t/dired-collapse ()
  "Collapse all expanded directories in dired listing."
  (beginning-of-buffer)
  (while (not (eobp))
    (forward-line)
    (when (dired-subtree--is-expanded-p)
      (dired-subtree-toggle)))
  (beginning-of-buffer))

(defun t/dired-locate ()
  "Locate the visited file in dired or t-sidebar."
  (interactive)
  (let* ((path (s-replace (t/project-root) "" (buffer-file-name)))
         (path (s-split "/" path))
         (path (remove "" path)))
    (let* ((sidebar (concat ":" (replace-regexp-in-string (expand-file-name "~") "~" (t/project-root)))))
      (if (get-buffer sidebar)
          (pop-to-buffer sidebar)
        (t-toggle-sidebar)))
    (when (t/prefix-arg-universal?)
      (t/dired-collapse))
    (t/dired-locate-path path)))

(defun t/toggle-window-cursor ()
  "Toggle cursor visibility in the window of the current buffer."
  (interactive)
  (internal-show-cursor (get-buffer-window (current-buffer))
                        (not (internal-show-cursor-p))))

(defun t/isodate ()
  (interactive)
  (let ((time (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil "UTC")))
    (if (interactive-p)
        (insert time)
      time)))

(defun t/date (&optional separator)
  (interactive)
  (let* ((sep (or separator "-"))
         (date (replace-regexp-in-string
                "-" sep (format-time-string "%Y-%m-%d" nil "UTC"))))
    (if (interactive-p)
        (insert date)
      date)))

(defun t/deploy-torgeir.dev ()
  (interactive)
  (let ((b "*t-deploy-torgeir_dev*"))
    (shell-command "cd ~/Projects/posts && git st && git df" b b)
    (pop-to-buffer b)
    (if (yes-or-no-p "Deploy changes?")
        (progn
          (shell-command
           (concat
            "cd ~/Projects/posts \
  && git add . \
  && git ci -m \"" (t/isodate) "\" \
  && git push") b b)
          (message "Deploy done."))
      (progn
        (with-current-buffer b (kill-buffer))
        (message "Cancelled.")))))

(defun t/preview-torgeir.dev ()
  (interactive)
  (+vterm/toggle nil)
  (term-send-raw-string
   (concat "cd ~/Projects/posts\C-m \
  open http://localhost:1313/\C-m\
  hugo server -p 1313 --navigateToChanged"
           (if (t/prefix-arg-universal?) "" " --buildDrafts")
           "\C-m")))

(defun t/chrome-urls ()
  "Combine all open chrome tabs into newline delimited string."
  (interactive)
  (shell-command-to-string
   "osascript -l JavaScript -e '[].slice.call(((app) => (app && app.windows) || [])(Application(\"Google Chrome\"))).map((w) => [w, [].slice.call(w.tabs)]).map(([_, tabs]) => tabs).flat().map((t, idx) => [\"[[\", t.url(), \"][Tab \", (idx + 1), \"]]\"].join(\"\")).join(\"\\n\")'"))

(defun t/say (text)
  "Speak TEXT using macos 'say' command."
  (if is-mac
      (shell-command (concat "say -v 'Karen' -r 250" ()))
    (error "'say' command not found")))

(defun t/speak-region (beg end)
  "Speak selected text using macos 'say' command."
  (interactive "r")
  (if is-mac
      (shell-command-on-region beg end "say -v 'Karen' -r 250")
    (error "'say' command not found")))

(defun t/org-heading ()
  "Fetch the org heading where the point is at."
  (interactive)
  (car (last (butlast (org-heading-components) 1))))

(defun t/insert-ox-hugo-slug ()
  "Insert a ox-hugo slug for the current org heading. Replace every
  non-letter-and-number with -, and remove double --."
  (interactive)
  (org-set-property
   "EXPORT_FILE_NAME"
   (s-replace-regexp
    "-+" "-"
    (s-replace-regexp
     "[^a-z0-9]" "-"
     (s-replace-all '((" a " . "-"))
                    (downcase (t/org-heading)))))))

(defun t/remove-consecutive-newlines ()
  (interactive)
  (read-only-mode -1)
  (save-excursion
    (beginning-of-buffer)
    (forward-line)
    (while (not (eobp))
      (beginning-of-line)
      (save-excursion
        (delete-horizontal-space)
        (delete-trailing-whitespace (pos-bol) (pos-eol)))
      (if (or
           ;; remove empty lines with leading * that
           ;; appear in a lot of mu4e emails
           (and (equal (+ 1 (pos-bol)) (pos-eol))
                (string-equal "*" (thing-at-point 'symbol)))
           ;; consecutive empty line
           (and (save-excursion (previous-line)
                                (and (bolp) (eolp)))
                (bolp)
                (eolp)))
          (kill-line 1)
        (forward-line))))
  (read-only-mode 1))

(defun t/is-darkmode ()
  (interactive)
  (if (not is-mac)
      t
    (equal
     "true"
     (t/trim-final-newline
      (t/run-osascript
       "tell application \"System Events\" to get dark mode of appearance preferences")))))

(defun t/toggle-system-appearence ()
  (interactive)
  (if (not is-mac)
      t
    (progn
      (t/run-osascript
       "tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode")
      (call-process "~/.config/alacritty/alacritty-toggle-appearance"))))

(defun t/load-system-theme ()
  (interactive)
  (disable-theme doom-theme)
  (load-theme
   (if (t/is-darkmode) t-system-theme-dark t-system-theme-light)))

(defun find-up (file)
  (interactive)
  (let ((current-dir default-directory)
        (project-root (projectile-project-root)))
    (while (and (not (file-exists-p (expand-file-name file current-dir)))
                (not (equal current-dir project-root)))
      (setq current-dir (file-name-directory (directory-file-name current-dir))))
    (if (file-exists-p (expand-file-name file current-dir))
        (expand-file-name file current-dir)
      nil)))

(provide 't-defuns)
;;; t-defuns.el ends here
