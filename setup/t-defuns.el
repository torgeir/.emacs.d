;;; t-defuns.el -*- lexical-binding: t; -*-
(defun t/exec-org-block (name vars)
  (save-excursion
    (goto-char (org-babel-find-named-block name))
    (org-babel-execute-src-block nil
                                 (org-babel-lob-get-info)
                                 (seq-map (lambda (var)
                                            (cons :var var))
                                          vars))))

(defun t/async-shell-command (name cmd &optional fn)
  "Execute `CMD' async, call `FN' with the result string."
  (lexical-let* ((fn fn)
                 (buf-name (format "*%s*" name))
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

(defun t/send-buffer-to-scala-repl ()
  "Send buffer to ensime repl, starts it if its not running"
  (interactive)
  (if (ensime-inf-process-live-p ensime-inf-buffer-name)
      (ensime-inf-eval-buffer)
    (progn
      (ensime-inf-switch)
      (other-window -1)
      (ensime-inf-eval-buffer))))

(defun t/send-region-to-scala-repl (start end)
  "Send region to ensime repl, starts it if its not running"
  (interactive "r")
  (if (ensime-inf-process-live-p ensime-inf-buffer-name)
      (ensime-inf-eval-region start end)
    (progn
      (ensime-inf-switch)
      (other-window -1)
      (ensime-inf-eval-region start end))))

(defun t/clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                  (mode-str (cdr cleaner))
                  (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(defun t/css-kill-value ()
  "kills the attribute of a css property"
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
  "per package modeline rename for mode"
  `(eval-after-load ,package-name
     '(defadvice ,mode (after t/rename-modeline activate)
        (setq mode-name ,new-name))))

(defun t/json-format ()
  "pretty prints json in selected region"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun t/build-tags ()
  "Build ctags file for projectile project, calls load-tags when done.

Effectively runs this for the current git project, e.g:

ctags -e -R --options=~/.emacs.d/ctags -f ~/.emacs.d/TAGS ~/.emacs.d/

The same can be done for the current folder only to place a TAGS file in it:

ctags -e -R .

Remember to build emacs --without-ctags and use the one from `brew' instead,
it's the one with the correct options needed to generate ctags that emacs
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
  "loads project tags into tag table"
  (message "loading project tags..")
  (visit-tags-table tags)
  (message "project tags loaded"))

(defun t/find-tag-at-point ()
  "goes to tag at point, builds and/or loads project TAGS file first"
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

(defun t/open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun t/hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

(defun t/hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

(defun t/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun t/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun t/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun t/paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (t/wrap-with-parens)
  (insert " ")
  (forward-char -1))

(defun t/untabify-buffer ()
  "Remove tabs in buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun t/indent-buffer ()
  "Correctly indents a buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

(defun t/cleanup-buffer-whitespace-and-indent ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (t/untabify-buffer)
  (when (fboundp 'ethan-wspace-clean-all) (ethan-wspace-clean-all))
  (t/indent-buffer))

(defun t/eval-region-or-last-sexp ()
  (interactive)
  (if (region-active-p) (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(defun t/eval-and-replace ()
  "Evaluate and replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

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

(defun t/copy-buffer-file-name ()
  (interactive)
  (add-string-to-kill-ring (file-name-nondirectory (buffer-file-name))))

(defun t/copy-buffer-file-path ()
  (interactive)
  (add-string-to-kill-ring (file-relative-name (buffer-file-name) (projectile-project-root))))

(defun t/previous-window ()
  "Skip back to previous window"
  (interactive)
  (call-interactively 'other-window))

(defun t/buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (with-current-buffer buffer-or-string
    major-mode))

(defvar t/cursors-direction 'down
  "Direction of cursor movement operations.")
(make-variable-buffer-local 't/cursors-direction)

(defun t/cursors-direction-is-up ()
  "Returns t if the current cursor movement direction is 'up."
  (eq t/cursors-direction 'up))

(defun t/cursors-direction-is-down ()
  "Returns t if the current cursor movement direction is 'down."
  (eq t/cursors-direction 'down))

(defun t/split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun t/split-window-below-and-move-there-dammit ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun t/evil-yank-to-end-of-line ()
  "Yank from point to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

(defun t/config-reload ()
  (interactive)
  (delete-file "~/.emacs.d/readme.el")
  (delete-file "~/.emacs.d/readme.elc")
  (load-file "~/.emacs.d/init.el"))

(defun t/face-color-b (attr)
  "Get `:background' color of `attr'"
  (face-attribute attr :background))

(defun t/face-color-f (attr)
  "Get `:foreground' color of `attr'"
  (face-attribute attr :foreground))

(defun t/switch-theme (theme)
  "Switch theme, disabling previously loaded"
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (when (called-interactively-p) (message "%s" theme)))

(defun t/toggle-theme-dark-light ()
  "Toggles between themes `doom-vibrant' and `spacemacs-light'"
  (interactive)
  (let* ((enabled-theme (car custom-enabled-themes))
         (next-theme (if (equal 'doom-vibrant enabled-theme)
                         'spacemacs-light
                       'doom-vibrant)))
    (t/switch-theme next-theme)))

(defvar *t-adjusted-font-size* t-font-size "Dynamically changed to adjust font with keybindings.")

(setq t-fonts (if is-mac
                  (list
                   "JetBrains Mono"
                   "DejaVu Sans Mono"
                   "Bitstream Vera Sans Mono"
                   "Code New Roman"
                   "Input Mono"
                   "Ubuntu Mono"
                   "Borg Sans Mono"
                   "Fira Code Retina"
                   )
                (list
                 "JetBrains Mono"
                 "Hack"
                 "Fira Code Retina"
                 "Inconsolata"
                 "Ubuntu Mono")))

(defun t/cycle-font ()
  "Cycle through list of fonts, setting the front most one."
  (interactive)
  (setq t-fonts
        (if (t/prefix-arg-universal?)
            (let ((first (car t-fonts))
                  (rest (cdr t-fonts)))
              (append (last t-fonts 1) (seq-take t-fonts (- (length t-fonts) 1))))
          (let ((first (car t-fonts))
                (rest (cdr t-fonts)))
            (append rest (list first)))))
  (t/set-font (car t-fonts) (not (called-interactively-p))))

(defun t/current-font ()
  "Grabs the current font. Prints message when called interactively."
  (interactive)
  (let ((font (face-attribute 'default :family)))
    (when (called-interactively-p) (message font))
    font))

(defun t/set-font (font &optional silence)
  "Change font."
  (when window-system
    (let ((sized-font (concat font "-" (number-to-string *t-adjusted-font-size*))))
      (set-face-attribute 'default nil :font sized-font)
      (when (not silence)
        (message "Font: %s." sized-font)))))

(defun t/reload-font ()
  "Reload font to make e.g. font size changes have effect."
  (interactive)
  (t/set-font (car t-fonts) t))

(defun t/fix-fira-ligatures ()
  (interactive)
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 ;;(45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 ;;(46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                            (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                            (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                            (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                            (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                            (91 . ".\\(?:]\\)")
                            (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                            (94 . ".\\(?:=\\)")
                            (119 . ".\\(?:ww\\)")
                            (123 . ".\\(?:-\\)")
                            (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                            (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
                            )
                 ))
                                          (dolist (char-regexp alist)
                                            (set-char-table-range composition-function-table (car char-regexp)
                                                                  `([,(cdr char-regexp) 0 font-shape-gstring])))))

(defun t/decrease-font-size ()
  (interactive)
  (setq *t-adjusted-font-size* (- *t-adjusted-font-size* 1))
  (t/reload-font))

(defun t/increase-font-size ()
  (interactive)
  (setq *t-adjusted-font-size* (+ *t-adjusted-font-size* 1))
  (t/reload-font))

(defun t/reset-font-size (&optional args)
  (interactive)
  (setq *t-adjusted-font-size* t-font-size)
  (t/reload-font)
  (text-scale-set 0))

(defun make-orgcapture-frame ()
  "@torgeir: credits https://github.com/jjasghar/alfred-org-capture/blob/master/el/alfred-org-capture.el
  Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16) (top . 400) (left . 300)))
  (select-frame-by-name "remember")
  (org-capture))

(defun t/sudo-edit (&optional arg)
  "Edit currently visited file as root.

   With a prefix ARG prompt for a file to visit.
   Will also prompt for a file to visit if current
   buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun server-remove-kill-buffer-hook ()
  ;; remove other clients-has-the-file-open-prompt
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))

(defun t/prefix-arg-universal? ()
  (equal '(4) current-prefix-arg))

(defun t/indent-after-paste (fn &rest args)
  (evil-start-undo-step)
  (let* ((u-prefix (t/prefix-arg-universal?))
         (current-prefix-arg (unless u-prefix current-prefix-arg))
         (args (if u-prefix (list nil) args)))
    (apply fn args)
    (unless u-prefix
      (indent-region (region-beginning) (region-end))))
  (evil-end-undo-step))

(advice-add 'yank :around #'t/indent-after-paste)
(advice-add 'evil-paste-before :around #'t/indent-after-paste)
(advice-add 'evil-paste-after :around #'t/indent-after-paste)

(defun t/comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun t/uniquify-lines ()
  "Remove duplicate adjacent lines in region or current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let ((beg (if (region-active-p) (region-beginning) (point-min)))
            (end (if (region-active-p) (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun t/sort-lines ()
  "Sort lines in region or current buffer"
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (sort-lines nil beg end)))

(defun t/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun t/switch-to-scratch-buffer ()
  "Switch to the `*scratch*' buffer. Create it first if needed."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*")))

(defun t/shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

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

(defun t/set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly 🚀"
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend))

;; TODO testing tweaks, remove if satisfying
;; (defun t/split-window-sensibly (&optional window)
;;   (cond
;;    ((and (> (window-width window)
;;             (* 2 (window-height window)))
;;          (window-splittable-p window 'horizontal))
;;     (with-selected-window window (split-window-right)))
;;    ((window-splittable-p window) (with-selected-window window
;;                                    (split-window-below)))))

(defun t/date-time ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00"
  (concat
   (format-time-string "%Y-%m-%dT%T")
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z"))))

(defun t/date-time-for-filename ()
  "Return date-time iso 8601 string suitable for filename"
  (replace-regexp-in-string ":" "." (t/date-time)))

(defun t/elpa-backup-directory ()
  "Returns the directory name of an elpa backup that would run now."
  (locate-user-emacs-file (format "elpa-backups/elpa-%s"
                                  (t/date-time-for-filename))))

(defun t/elpa-backup ()
  "Backup the current elpa folder to elpa-backups."
  (interactive)
  (copy-directory
   (locate-user-emacs-file "elpa")
   (t/elpa-backup-directory)
   t ; keep last modified time
   t ; create parents
   ))

(defun t/upgrade-packages ()
  "Upgrade packages after backing up the current elpa files."
  (interactive)
  (message "Backing up elpa/")
  (t/elpa-backup)
  (message "Backing up elpa/: done.")
  (paradox-upgrade-packages))

(defun t/current-line-ends-in-comma ()
  "Return whether the current line is suffixed with ','"
  (save-excursion
    (end-of-line)
    (looking-back ",\s*")))

(defun t/prev-line-ends-in-comma ()
  "Return whether the current line is suffixed with ','"
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (looking-back ",\s*")))

(defun t/next-line-ends-in-comma ()
  "Return whether the current line is suffixed with ','"
  (save-excursion
    (forward-line)
    (end-of-line)
    (looking-back ",\s*")))

(defun t/move-line-up (arg)
  "Move the current line(s) down one line."
  (interactive "P")
  (if (or (not arg) (>= arg 0))
      (let ((reg-or-lin (if (region-active-p) "'<" "."))
            (reactivate-region (if (region-active-p) "gv=gv" ""))
            (num (if arg (+ arg 1) 2)))
        (execute-kbd-macro
         (concat ":m" reg-or-lin "-" (number-to-string num) (kbd "RET") reactivate-region)))
    (move-line-or-region (- arg))))

(defun t/move-line-down (arg)
  "Move the current line(s) down one line."
  (interactive "P")
  (if (or (not arg) (>= arg 0))
      (let ((reg-or-lin (if (region-active-p) "'>" "."))
            (reactivate-region (if (region-active-p) "gv=gv" ""))
            (num (if arg arg 1)))
        (execute-kbd-macro
         (concat ":m" reg-or-lin "+" (number-to-string num) (kbd "RET") reactivate-region)))
    (backward-move-line-or-region (- arg))))


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
  "Get project root without throwing"
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

(defun t/volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (setq-local kill-buffer-query-functions
              (remq 'process-kill-buffer-query-function
                    kill-buffer-query-functions))
  (let ((buffer-modified-p nil))
    (kill-this-buffer)))

(defun t/volatile-kill-buffer-and-window ()
  "Kill current buffer and the window unconditionally."
  (interactive)
  (setq-local kill-buffer-query-functions
              (remq 'process-kill-buffer-query-function
                    kill-buffer-query-functions))
  (let ((buffer-modified-p nil))
    (kill-buffer-and-window)))

(defun t/grab-chrome-url ()
  "Grab the frontmost url out of chrome using `org-mac-grab-link'"
  (interactive)
  (when-let ((chrome-url (org-mac-chrome-get-frontmost-url))
             (_ (string-match "\\\[\\\[\\(.*\\)\\\]\\\[" chrome-url)))
    (match-string 1 chrome-url)))

(defun t/browse-chrome-url-in-eww ()
  "Open the frontmost chrome url in `eww'. "
  (interactive)
  (eww (t/grab-chrome-url)))

(defun t/get-url (url)
  "Get url synchronously."
  (with-current-buffer (url-retrieve-synchronously url)
    (buffer-string)))

(defun t/fetch (url)
  "Insert url contents in current buffer. Drops headers and 2x empty lines before content."
  (interactive "sfetch url:")
  (insert (t/get-url url))
  (goto-char (point-min))
  (kill-sentence)
  (kill-line)
  (kill-line))

(defun t/fetch-chrome-url ()
  "Insert contents of frontmost url of chrome in buffer."
  (interactive)
  (t/fetch (t/grab-chrome-url)))

(defun t/browse-url-at-point ()
  (interactive)
  (if (t/prefix-arg-universal?)
      (let ((browse-url-browser-function 'eww-browse-url)) ;; force eww
        (browse-url-at-point))
    (let ((browse-url-browser-function 'browse-url-default-browser))
      (cond ((equal major-mode 'eww-mode) (eww-browse-with-external-browser))
            ((equal major-mode 'elfeed-search-mode) (elfeed-search-browse-url))
            ((equal major-mode 'elfeed-show-mode) (browse-url (elfeed-entry-link elfeed-show-entry)))
            ((equal major-mode 'gnus-summary-mode) (gnus-summary-browse-url))
            ((equal major-mode 'magit-status-mode) (call-interactively 'forge-browse-dwim))
            ((equal major-mode 'hackernews-mode) (hackernews-browse-url-action (button-at (point))))
            (t (call-interactively 'browse-url-at-point))))))

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
  "Echo the face at point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun t/font-lock-test-faces ()
  "Outputs test strings with all font lock faces to show colors."
  (interactive)
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
    (end-of-buffer)
    (let ((current-string (concat "\n" (symbol-name face)))
          (is-font-lock font-lock-mode))
      (when is-font-lock (font-lock-mode 0))
      (put-text-property 1 (length current-string) 'face face current-string)
      (insert current-string)
      (when is-font-lock (font-lock-mode 1)))))

(defun t/find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 20 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(defun t/run-osascript (s)
  "Run applescript."
  (shell-command (format "osascript -e '%s'" s)))

(defun t/osascript-activate (app)
  "Run applescript to activate application."
  (t/run-osascript (format "tell application \"%s\" to activate" app)))

(defun t/osascript-show-url (url)
  (t/run-osascript
   (format "tell application \"Google Chrome\" to set URL of active tab of window 1 to \"%s\"" url)))

(defun t/open-in-intellij ()
  "Opens current file in IntelliJ IDEA."
  (interactive)
  (shell-command
   (let* ((cmd "/Applications/IntelliJ\\ IDEA.app/Contents/MacOS/idea %s")
          (args " --line %d %s")
          (root (t/project-root))
          (file-name (buffer-file-name)))
     (if file-name
         (format (concat cmd args)
                 root
                 (line-number-at-pos)
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

(defun t/mobile-inbox-count ()
  "Counts the number of items in `org-mobile-inbox-for-pull'."
  (let ((inbox-file org-mobile-inbox-for-pull))
    (when (file-exists-p inbox-file)
      (with-temp-buffer
        (insert-file-contents inbox-file)
        (let ((matches (count-matches "^\*+ " (point-min) (point-max))))
          (when (> matches 0) matches))))))

(defun t/get-string-from-file (file-path)
  "Return `file-path's file content."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun t/re-seq (regexp string)
  "Get a list of all regexp matches (from the first group) in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 1 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun t/re-seq-in-file (regex file)
  (t/re-seq regex (t/get-string-from-file (t/user-emacs-file file))))

(defun t/fn-prefix (prefix module)
  (let ((module (if (stringp module) module (symbol-name module))))
    (mapcar
     (lambda (fn-name)
       (intern fn-name))
     (t/re-seq-in-file (concat "defun " "\\(" prefix "\\)" "[ \\)]")
                       (concat "setup/" module ".el")))))

(defun t/call-fns (fns)
  (mapcar 'funcall fns))

(defun t/call-prefix (prefix module)
  (t/call-fns (t/fn-prefix prefix (symbol-name module))))

(defun t/call-init (prefix-fmt pkg)
  (let ((fn (intern (format prefix-fmt pkg))))
    (when (fboundp fn)
      (when *t-debug-init*
        (message "t/call-init: %s" fn))
      (funcall fn))))

(defun t/buffer-finished-p (b)
  "Return non-nil value if the buffer has an ended process."
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

(defun t/reveal-file ()
  (interactive)
  (let ((origin-buffer-file-name (buffer-file-name)))
    (dired-sidebar-point-at-file
     origin-buffer-file-name
     (t/project-root))))

(defun t/backward-down-sexp ()
  "Move backward to the start of the previous sexp."
  (interactive)
  (cond
   ((looking-back "^\s*?") (backward-word))
   ((or (looking-back " ")
        (looking-back ")")) (backward-char))
   ((and (looking-back "(")
         (looking-at "(")) (backward-char))
   ((looking-back "(") (evil-cp-backward-up-sexp))
   (t (sp-backward-sexp))))

(defun t/forward-down-sexp ()
  "Move forward to the end of the previous sexp."
  (interactive)
  (cond
   ((looking-at "\"") (forward-sexp))
   ((looking-at "\)\)$") (forward-char))
   ((or (looking-at "\s(")
        (looking-at "(")) (forward-char))
   ((and (looking-at "[^\s]")
         (not (looking-at ".$"))) (forward-sexp))
   ((looking-at "\s*[^\s]") (evil-forward-word-begin))
   ((or (looking-at "\s)")
        (looking-at ".$")) (evil-cp-next-opening))
   (t (sp-forward-sexp))))

(defun t/forward-sexp ()
  (interactive)
  (if (looking-at "(")
      (evil-jump-item)
    (if (looking-at ")")
        (progn
          (evil-emacs-state)
          (forward-char)
          (sp-forward-sexp)
          (evil-normal-state))
      (evil-cp-up-sexp))))

(defun t/backward-sexp ()
  (interactive)
  (if (looking-at ")")
      (evil-jump-item)
    (if (looking-at "(")
        (progn
          (evil-emacs-state)
          (sp-backward-sexp)
          (evil-normal-state))
      (evil-cp-backward-up-sexp))))

(defun t/find-files-emacs-init-files ()
  (interactive)
  (let ((default-directory user-emacs-directory))
    (counsel-projectile-find-file-dwim)))

(defun t/newline-expand-braces ()
  "Newline like `evil-ret', but expand (), [] and {} with newline in between, and indent accordingly."
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

(defun t/recompile-elpa ()
  "Recompile the elpa/ directory to resolve byte compilation issues."
  (interactive)
  (byte-recompile-directory (expand-file-name (t/user-emacs-file "elpa")) 0))

(defun t/describe (&optional sat)
  "Describe functions, features, symbols, or run help-apropos if it's not found."
  (interactive)
  (let ((s (or sat (symbol-at-point))))
    (cond ((fboundp s) (describe-function s))
          ((featurep s) (describe-package s))
          ((symbolp s) (describe-variable s))
          (t (call-interactively 'counsel-apropos)))))

(defun t/unbind (fn-or-s)
  "Unbind function or symbol depending on type."
  (interactive)
  (cond ((fboundp fn-or-s) (fmakunbound fn-or-s))
        ((symbolp fn-or-s) (makunbound fn-or-s))
        (t (message "Can't unbind %s, not a fboundp or symbolp" fn-or-s))))

(defun t/add-to-list (l item-or-items)
  "Adds items to the list `l'."
  (if (listp item-or-items)
      (dolist (item item-or-items) (add-to-list l item t))
    (add-to-list l item-or-items))
  l)

(defun t/toggle-line-numbers ()
  "Toggle line numbers on or off."
  (interactive)
  (nlinum-mode (if nlinum-mode 0 1)))

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
  "Clone a github repo to `~/Code/<repo-name>'."
  (interactive "sClone repository: ")
  (require 'magit)
  (when-let* ((repo-name (or (and (s-starts-with? "https://github.com" repo)
                                  (replace-regexp-in-string "https://github\\.com/\\([^/]+\\)/\\([^/]+\\)/?.*?$" "\\1/\\2" repo))
                             (and (s-ends-with? ".git" repo)
                                  (replace-regexp-in-string "git@github.com:\\(.+\\).git" "\\1" repo))))
              (dir (expand-file-name (format "~/Code/%s/" repo-name))))
    (when (not (file-exists-p dir))
      (message "Cloning %s.." repo repo-name)
      (magit-run-git-async "clone"
                           (format "git@github.com:%s.git" repo-name)
                           (magit-convert-filename-for-git dir))
      (message "Cloning %s.. ok." repo repo-name))
    (dired dir)))

(defun t/set-company-backends (&rest backends)
  "Set company backends."
  (make-local-variable 'company-backends)
  (setq-local company-backends backends))

(defun t/add-company-backends (&rest backends)
  "Add list of grouped company backends."
  (make-local-variable 'company-backends)
  (setq-local company-backends (t/company-backends backends)))

(defun t/add-company-backends-hook (mode-hook &rest backends)
  "Add list of grouped company backends for `mode-hook'."
  (add-hook mode-hook
            (lambda nil
              (apply 't/add-company-backends backends))))

(defun t/visit-git-link-pulls ()
  "Navigate to /pulls for the current git repo."
  (interactive)
  (autoload 'git-link--remote-host "git-link")
  (autoload 'git-link--remote-dir "git-link")
  (autoload 'git-link--select-remote "git-link")
  (browse-url (concat "https://"
                      (git-link--remote-host (git-link--select-remote)) "/"
                      (git-link--remote-dir (git-link--select-remote)) "/pulls")))

(defun t/projectile-dired ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: "
         projects
         :action 'dired)
      (user-error "No projects found"))))

(defun t/projectile-desktop ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: "
         projects
         :action (lambda (project)
                   (when (t/desktop-project-name)
                     (t/desktop-save))
                   (let ((default-directory project))
                     (t/desktop-restore))))
      (user-error "No projects found"))))

(defun t/projectile-magit-status ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: "
         projects
         :action (lambda (project)
                   (let ((default-directory project))
                     (magit-status project))))
      (user-error "No projects found"))))

(defun t/projectile-ag ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: "
         projects
         :action (lambda (project)
                   (let ((default-directory project))
                     (counsel-projectile-ag))))
      (user-error "No projects found"))))

(defun t/projectile-rg ()
  (interactive)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: "
         projects
         :action (lambda (project)
                   (let ((default-directory project))
                     (counsel-projectile-rg))))
      (user-error "No projects found"))))

(defun t/projectile-visit-git-link-pulls ()
  (interactive)
  (require 'ghub)
  (let ((projects (projectile-load-known-projects)))
    (if projects
        (projectile-completing-read
         "Visit pulls for project: "
         projects
         :action (lambda (project)
                   (let* ((default-directory (expand-file-name project))
                          (url (magit-get "remote" (magit-get-remote "master") "url"))
                          (id (replace-regexp-in-string "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1" url)))
                     (browse-url (format "https://github.com/%s/pulls" id)))))
      (user-error "No projects found"))))

(defun t/margins-global (l &optional r)
  "Set global frame margins."
  (interactive "nWidth: ")
  (setq-default left-margin-width l
                right-margin-width (or r l))
  (set-window-buffer nil (current-buffer)))

(defun t/margins-local (l &optional r)
  "Set buffer local frame margin."
  (interactive "nWidth: ")
  (setq-local left-margin-width l)
  (setq-local right-margin-width (or r l))
  (set-window-buffer nil (current-buffer)))

(defun t/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

(defun t/last-used-window-buffer ()
  "Switch to the window that displays the most recently selected buffer."
  (interactive)
  (let* ((buffers (delq (current-buffer) (buffer-list (selected-frame))))
         (windows (delq (selected-window) (delq nil (mapcar #'get-buffer-window buffers)))))
    (if windows
        (select-window (car windows))
      (message "no suitable window to switch to"))))

(defun t/safe-restart-emacs ()
  "Restart emacs if config parses correctly."
  (interactive)
  (if (eq last-command this-command)
      (save-buffers-kill-terminal)
    (require 'async)
    (async-start
     (lambda () (shell-command-to-string
                 "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
     `(lambda (output)
        (if (string-match "-OK-" output)
            (when ,(called-interactively-p 'any)
              (message "Ok."))
          (switch-to-buffer-other-window "*startup error*")
          (delete-region (point-min) (point-max))
          (insert output)
          (search-backward "Error!"))))))

(defun t/word-at-point ()
  "Returns word under cursor."
  (thing-at-point 'word t))

(defun t/ip ()
  (interactive)
  (let ((ip (-> "dig +short myip.opendns.com @resolver1.opendns.com" shell-command-to-string s-trim)))
    (message ip)
    (kill-new ip)))

(defun t/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive (list
                (string-to-number
                 (read-string (format "Transparency Value 0 - 100 opaque: " (thing-at-point 'symbol))
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


(defun t/complete-with-helm-then (list fn)
  "Complete from list with helm, e.g. (t/complete-with-helm-then '(one two) 'insert)"
  (interactive)
  (helm :sources (helm-build-sync-source "Choose from list."
                                         :action fn
                                         :candidates list)))


(defun t/locally-disable-cursor ()
  "Locally disable cursor in buffer."
  (interactive)
  (make-local-variable 'cursor-type)
  (make-local-variable 'evil-normal-state-cursor)
  (setq-local cursor-type 'nil)
  (setq-local evil-normal-state-cursor 'nil))

(defun t/random-line ()
  "Goto a random line in the buffer. Useful trying out a random package."
  (interactive)
  (push-mark)
  (goto-char (point-min))
  (forward-line (random (count-lines (point-min) (point-max)))))

(defun t/decrease-frame-width ()
  "Decrease emacs frame size horizontally"
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-width frame (- (frame-width frame) 4))))

(defun t/increase-frame-width ()
  "Increase emacs frame size horizontally"
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-width frame (+ (frame-width frame) 4))))

(defun t/decrease-frame-height ()
  "Decrease emacs frame size vertically"
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-height frame (- (frame-height frame) 4))))

(defun t/increase-frame-height ()
  "Increase emacs frame size vertically"
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-height frame (+ (frame-height frame) 4))))

(defun t/move-frame-right ()
  "Moves emacs frame right"
  (interactive)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left)))
    (set-frame-parameter frame 'left (+ left 40))))

(defun t/move-frame-left ()
  "Moves emacs frame left"
  (interactive)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left)))
    (set-frame-parameter frame 'left (- left 40))))

(defun t/move-frame-up ()
  "Moves emacs frame up"
  (interactive)
  (let* ((frame (selected-frame))
         (top (frame-parameter frame 'top)))
    (set-frame-parameter frame 'top (- top 40))))

(defun t/move-frame-down ()
  "Moves emacs frame down"
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
(diary-last-day-of-month '(2 28 2017))"

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

(defun t/search-cheat-sh ()
  "Search `http://cheat.sh/' for help on commands and code."
  (interactive)
  (ivy-read "Command or Topic: "
            (process-lines "curl" "--silent" "http://cheat.sh/:list?T&q")
            :require-match t
            :sort t
            :history 't/search-cheat-sh
            :action (lambda (input)
                      (eww (concat "http://cheat.sh/" input "?T&q")))
            :caller 't/search-cheat-sh))

(defun t/toggle-dedicated-window ()
  (interactive)
  (let ((dedicated (not (window-dedicated-p (selected-window)))))
    (set-window-dedicated-p (selected-window) dedicated)
    (set-window-parameter (selected-window) 'no-delete-other-windows dedicated)
    (message (if dedicated "Window dedicated." "Window not dedicated."))))

(defun t/compile (dir)
  (interactive "DCompile in directory: ")
  (let ((default-directory (expand-file-name dir)))
    (compile "make")))

(defun t/where-am-i ()
  (interactive)
  (let ((l (xref-location-marker
            (xref-make-file-location
             (buffer-file-name)
             (line-number-at-pos)
             (current-column)))))
    (xref--show-pos-in-buf l (marker-buffer l))))

(defun t/format-xml ()
  "Reformat and replace buffer content using xmllint. Install on macos using `brew install libxml2`"
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "xmllint --format -"
   (current-buffer) t))

(provide 't-defuns)