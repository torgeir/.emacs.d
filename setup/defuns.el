(defun t/send-region-to-nodejs-repl-process (start end)
  "Send region to `nodejs-repl' process."
  (interactive "r")
  (save-selected-window
    (save-excursion (nodejs-repl)))
  (comint-send-region (get-process nodejs-repl-process-name)
                      start end))

(defun t/send-buffer-to-nodejs-repl-process ()
  "Send buffer to `nodejs-repl process."
  (interactive)
  (t/send-region-to-nodejs-repl-process (point-min) (point-max)))

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
  "build ctags file for projectile project, calls load-tags when done"
  (interactive)
  (message "building project tags..")
  (lexical-let* ; so lambdas create closures
      ((root (projectile-project-root))
       (ctags (expand-file-name "~/.emacs.d/ctags"))
       (tags (concat root "TAGS"))
       (process (start-process-shell-command
                 "build ctags asynchronously"
                 "*ctags async*"
                 (concat
                  "ctags -e -R"       ; etags mode, recurse
                  " --options=" ctags ; use global config
                  " -f " tags " " ; put it in project/TAGS
                  root))))
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
         (tags (concat root "TAGS")))
    (if (file-exists-p tags) (load-tags tags) (t/build-tags))
    (etags-select-find-tag-at-point)))

(defun t/ido-find-tag ()
  "Find a tag using ido"
  (interactive)
  (tags-completion-table)
  (let (tag-names)
    (mapatoms (lambda (x)
                (push (prin1-to-string x t) tag-names))
              tags-completion-table)
    (etags-select-find (ido-completing-read "Tag: " tag-names))))

(defun t/ido-go-straight-home ()
  (interactive)
  (cond
   ((looking-back "/") (insert "~/"))
   (:else (call-interactively 'self-insert-command))))

(defun copy-to-clipboard (text &optional push)
  "Copy text to os clipboard. Cygwin uses cygutils-extra's `putclip`. Mac uses builtin pbcopy."
  (let* ((process-connection-type nil)
         (copy-cmd (if is-cygwin "putclip" "pbcopy"))
         (proc (start-process copy-cmd "*Messages*" copy-cmd)))
    (process-send-string proc text)
    (process-send-eof proc)))

(defun t/open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-06-12"
  (interactive)
  (cond
   (is-win (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   (is-cygwin (shell-command (concat "cygstart " (shell-quote-argument default-directory))))
   (is-mac (shell-command "open ."))
   (is-linux (shell-command "xdg-open ."))))

(defun t/open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
    (beginning-of-line))
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun t/open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
    (end-of-line))
  (newline-and-indent))

(defun t/hippie-expand-no-case-fold ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand nil)))

(defun t/hippie-expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-expand-line-all-buffers)))
    (end-of-line)
    (hippie-expand nil)))

(defun t/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (t/duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (t/duplicate-region 1 beg end))))
    (t/duplicate-current-line arg)
    (one-shot-keybinding "d" 't/duplicate-current-line))
  (forward-line 1))

(defun t/duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun t/duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (save-excursion
    (when (eq (point-at-eol) (point-max))
      (goto-char (point-max))
      (newline)
      (forward-char -1))
    (t/duplicate-region num (point-at-bol) (1+ (point-at-eol)))))

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

(defun t/paredit-wrap-round-from-behind (ignore)
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
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
  (ethan-wspace-clean-all)
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

(defun t/join-lines ()
  "join adjacent lines"
  (interactive)
  (join-line -1))

(defun t/kill-and-join-forward (&optional arg)
  "kills line and joins the next line, without the whitespace"
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

(defun t/isearch-delete-me ()
  (interactive)
  (delete-char (- (length isearch-string)))
  (isearch-exit))

(defun t/quit-other-window ()
  (interactive)
  (other-window 1)
  (quit-window))

(defun t/lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun t/smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun t/sp--create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. thx @bodil"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun t/delete-frame-or-hide-last-remaining-frame ()
  "Delete the selected frame. If the last one, hide it instead."
  (interactive)
  (condition-case nil
      (delete-frame)
    (error (if (window-system)
               (ns-do-hide-emacs)
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
  (other-window -1))

(defun t/prefix-with-leader (key)
  "Prefixes `key' with `leader' and a space, e.g. 'SPC m'"
  (concat leader " " key))

(defun t/declare-prefix (prefix name &optional key fn &rest bindings)
  "Declares which-key `prefix' and a display `name' for the prefix.
   Sets up keybindings for the prefix."
  (which-key-declare-prefixes (t/prefix-with-leader prefix) name)
  (let ((init-prefix prefix))
    (while key
      (evil-leader/set-key (concat init-prefix key) fn)
      (setq key (pop bindings)
            fn (pop bindings)))))

(defun t/declare-prefix-for-mode (mode prefix name &optional key fn &rest bindings)
  "Declares which-key `prefix' and a display `name' for the prefix only in `mode`.
   Sets up keybindings for the prefix."
  (which-key-declare-prefixes-for-mode mode (t/prefix-with-leader prefix) name)
  (let ((init-prefix prefix))
    (while key
      (evil-leader/set-key-for-mode mode (concat init-prefix key) fn)
      (setq key (pop bindings)
            fn (pop bindings)))))

(defun t/declare-state (prefix name &optional key fn &rest bindings)
  "Micro state that temporarily overlays a new key map, kinda like hydra"
  (lexical-let* ((keymap (make-sparse-keymap))
                 (init-prefix prefix))
    (while key
      (bind-key key fn keymap)
      (setq key (pop bindings)
            fn (pop bindings)))
    (evil-leader/set-key
      (concat init-prefix key)
      (lambda ()
        (interactive)
        (set-temporary-overlay-map keymap t)))))

(defun buffer-mode (buffer-or-string)
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

(defun t/cursors-message (str)
  "Print message with added `str' and cursor direction."
  (message (concat "cursor " str ": " (symbol-name t/cursors-direction))))

(defun t/cursor-unmark ()
  ""
  (interactive)
  (if (t/cursors-direction-is-down)
      (progn
        (mc/unmark-next-like-this)
        (t/cursors-message "unmark"))
    (progn
      (mc/unmark-previous-like-this)
      (t/cursors-message "unmark"))))

(defun t/cursor-down ()
  "Marks `next-like-this' if the `t/cursors-direction' is 'down.
   Sets `t/cursors-direction' to 'down if `t/cursors-direction' is 'up."
  (interactive)
  (if (t/cursors-direction-is-down)
      (progn
        (mc/mark-next-like-this 1)
        (t/cursors-message "mark"))
    (progn
      (setq t/cursors-direction 'down)
      (t/cursors-message "direction"))))

(defun t/cursor-up ()
  "Marks `previous-like-this' if the `t/cursors-direction' is 'up.
   Sets `t/cursors-direction' to 'up if `t/cursors-direction' is 'down."
  (interactive)
  (if (t/cursors-direction-is-up)
      (progn
        (mc/mark-previous-like-this 1)
        (t/cursors-message "mark"))
    (progn
      (setq t/cursors-direction 'up)
      (t/cursors-message "direction"))))


(defun t/cursor-down-skip ()
  "Skips to `next-like-this' if `t/cursors-direction' is 'down.
   Unmarks `previous-like-this' if `t/cursors-direction' is 'up"
  (interactive)
  (if (t/cursors-direction-is-up)
      (progn
        (mc/unmark-previous-like-this)
        (t/cursors-message "unmark"))
    (progn
      (mc/skip-to-next-like-this)
      (t/cursors-message "skip"))))

(defun t/cursor-up-skip ()
  "Skips to `previous-like-this' if `t/cursors-direction' is 'up.
   Unmarks `next-like-this' if `t/cursors-direction' is 'down"
  (interactive)
  (if (t/cursors-direction-is-down)
      (progn
        (mc/unmark-next-like-this)
        (t/cursors-message "unmark"))
    (progn
      (mc/skip-to-previous-like-this)
      (t/cursors-message "skip"))))

(defun t/split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun t/split-window-below-and-move-there-dammit ()
  (interactive)
  (split-window-below)
  (windmove-down))

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
    (set-frame-height frame (- (frame-height frame) 2))))

(defun t/increase-frame-height ()
  "Increase emacs frame size vertically"
  (interactive)
  (let ((frame (selected-frame)))
    (set-frame-height frame (+ (frame-height frame) 2))))

(provide 'defuns)
