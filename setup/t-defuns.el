(defun t/tab-properly ()
  (interactive)
  (let ((yas-fallback-behavior 'return-nil))
    (unless (yas-expand)
      (simplezen-expand-or-indent-for-tab)
      (if (looking-back "^\s*")
          (back-to-indentation)))))

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
       (tags (shell-quote-argument (concat root "TAGS")))
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
         (tags (shell-quote-argument (concat root "TAGS"))))
    (if (file-exists-p tags) (t/load-tags tags) (t/build-tags))
    (when (find-tag-default)
      (etags-select-find-tag-at-point))))

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
  (other-window -1))

(defun t/prefix-with-leader (key)
  "Prefixes `key' with `leader' and a space, e.g. 'SPC m'"
  (concat *user-leader* " " key))

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

(defun t/cursor-down ()
  "Marks `next-like-this' if the `t/cursors-direction' is 'down.
   Sets `t/cursors-direction' to 'down if `t/cursors-direction' is 'up."
  (interactive)
  (when (not evil-mc-mode)
    (turn-on-evil-mc-mode))
  (if (t/cursors-direction-is-down)
      (progn
        (evil-mc-make-and-goto-next-match)
        (t/cursors-message "mark"))
    (progn
      (setq t/cursors-direction 'down)
      (t/cursors-message "direction"))))

(defun t/cursor-up ()
  "Marks `previous-like-this' if the `t/cursors-direction' is 'up.
   Sets `t/cursors-direction' to 'up if `t/cursors-direction' is 'down."
  (interactive)
  (when (not evil-mc-mode)
    (turn-on-evil-mc-mode))
  (if (t/cursors-direction-is-up)
      (progn
        (evil-mc-make-and-goto-prev-match)
        (t/cursors-message "mark"))
    (progn
      (setq t/cursors-direction 'up)
      (t/cursors-message "direction"))))

(defun t/cursor-down-skip ()
  "Skips to `next-like-this' if `t/cursors-direction' is 'down.
   Unmarks `previous-like-this' if `t/cursors-direction' is 'up"
  (interactive)
  (when (not evil-mc-mode)
    (turn-on-evil-mc-mode))
  (if (t/cursors-direction-is-up)
      (progn
        (evil-mc-skip-and-goto-prev-cursor)
        (t/cursors-message "unmark"))
    (progn
      (evil-mc-skip-and-goto-next-match)
      (t/cursors-message "skip"))))

(defun t/cursor-up-skip ()
  "Skips to `previous-like-this' if `t/cursors-direction' is 'up.
   Unmarks `next-like-this' if `t/cursors-direction' is 'down"
  (interactive)
  (when (not evil-mc-mode)
    (turn-on-evil-mc-mode))
  (if (t/cursors-direction-is-down)
      (progn
        (evil-mc-skip-and-goto-next-cursor)
        (t/cursors-message "unmark"))
    (progn
      (evil-mc-skip-and-goto-prev-match)
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

(defun t/move-frame-right ()
  "Moves emacs frame right"
  (interactive)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left)))
    (set-frame-parameter frame 'left (+ left 20))))

(defun t/move-frame-left ()
  "Moves emacs frame left"
  (interactive)
  (let* ((frame (selected-frame))
         (left (frame-parameter frame 'left)))
    (set-frame-parameter frame 'left (- left 20))))

(defun t/move-frame-up ()
  "Moves emacs frame up"
  (interactive)
  (let* ((frame (selected-frame))
         (top (frame-parameter frame 'left)'top))
    (set-frame-parameter frame 'top (- top 20))))

(defun t/move-frame-down ()
  "Moves emacs frame down"
  (interactive)
  (let* ((frame (selected-frame))
         (top (frame-parameter frame 'top)))
    (set-frame-parameter frame 'top (+ top 20))))

(defun config-reload () (interactive) (load-file "~/.emacs.d/init.el"))
(defun config-edit-init () (interactive) (find-file "~/.emacs.d/init.el"))
(defun config-edit-org () (interactive) (find-file "~/.emacs.d/setup/t-org.el"))
(defun config-edit-sane-defaults () (interactive) (find-file "~/.emacs.d/setup/t-sane-defaults.el"))
(defun config-edit-defuns () (interactive) (find-file "~/.emacs.d/setup/t-defuns.el"))
(defun config-edit-keys () (interactive) (find-file "~/.emacs.d/setup/t-keys.el"))
(defun config-edit-mac () (interactive) (find-file "~/.emacs.d/setup/t-mac.el"))
(defun config-edit-langs () (interactive) (find-file "~/.emacs.d/setup/t-langs.el"))
(defun config-edit-snippets () (interactive) (find-file "~/.emacs.d/snippets/"))

(defmacro t/macro-helm-ag-insert (thing fn)
  `(lambda ()
     (interactive)
     (setq-local helm-ag-insert-at-point ,thing)
     (,fn)
     (setq-local helm-ag-insert-at-point nil)))

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
  (load-theme theme t))

(defun t/toggle-theme-dark-light ()
  "Toggles between themes `spacemacs-dark' and `spacemacs-light'"
  (interactive)
  (let* ((enabled-theme (car custom-enabled-themes))
         (next-theme (if (equal 'spacemacs-dark enabled-theme) 'spacemacs-light 'spacemacs-dark)))
    (t/switch-theme next-theme)))

;; mac/win friendly font
(defun t/reload-font ()
  (interactive)
  (when window-system
    (setq t/default-font (concat (if is-mac "Fira Code Retina-" "Inconsolata-")
                                 (number-to-string t/default-font-size)))
    (set-face-attribute 'default nil :font t/default-font)))
(defun t/decrease-font-size ()
  (interactive)
  (setq t/default-font-size (- t/default-font-size 1))
  (t/reload-font))
(defun t/increase-font-size ()
  (interactive)
  (setq t/default-font-size (+ t/default-font-size 1))
  (t/reload-font))
(defun t/reset-font-size ()
  (interactive)
  (setq t/default-font-size t/initial-font-size)
  (t/reload-font)
  (text-scale-set 0))

(defun make-orgcapture-frame ()
  "@torgeir: credits https://github.com/jjasghar/alfred-org-capture/blob/master/el/alfred-org-capture.el
  Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "remember") (width . 80) (height . 16) (top . 400) (left . 300)))
  (select-frame-by-name "remember")
  (org-capture))

(defun t/refile-to (file headline)
  "Move current headline to specified location"
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile nil nil (list headline file nil pos))))

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


;; remove other clients-has-the-file-open-prompt
(defun server-remove-kill-buffer-hook ()
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

(defadvice yank (around t/advice-indent-paste-before activate)
  "Advice to intent text after pasting with `yank'.
   Use `c-u yank' to prevent it."
  (evil-start-undo-step)
  (let* ((prefix current-prefix-arg)
         (got-universal-prefix (equal '(4) prefix)))
    (ad-set-arg 0 (unless got-universal-prefix prefix))
    ad-do-it
    (unless got-universal-prefix
      (indent-region (region-beginning)
                     (region-end))))
  (evil-end-undo-step))

(defadvice yank-pop (around t/advice-indent-paste-before activate)
  "Advice to intent text after pasting with `yank-pop'.
   Use `c-u yank-pop' to prevent it."
  (evil-start-undo-step)
  (let* ((prefix current-prefix-arg)
         (got-universal-prefix (equal '(4) prefix)))
    (ad-set-arg 0 (unless got-universal-prefix prefix))
    ad-do-it
    (unless got-universal-prefix
      (indent-region (region-beginning)
                     (region-end))))
  (evil-end-undo-step))

(defadvice evil-paste-before (around t/advice-indent-paste-before activate)
  "Advice to intent text after pasting with `P'.
   Use `c-u P' to prevent it."
  (evil-start-undo-step)
  (let* ((prefix current-prefix-arg)
         (got-universal-prefix (equal '(4) prefix)))
    (ad-set-arg 0 (unless got-universal-prefix prefix))
    ad-do-it
    (unless got-universal-prefix
      (indent-region (region-beginning)
                     (region-end))))
  (evil-end-undo-step))

(defadvice evil-paste-after (around t/advice-indent-paste-after activate)
  "Advice to intent text after pasting with `p'.
   Use `c-u p' to prevent it."
  (evil-start-undo-step)
  (let* ((prefix current-prefix-arg)
         (got-universal-prefix (equal '(4) prefix)))
    (ad-set-arg 0 (unless got-universal-prefix prefix))
    ad-do-it
    (unless got-universal-prefix
      (indent-region (region-beginning)
                     (region-end))))
  (evil-end-undo-step))

;; Remove an advice: disable it, then activate it?
;; (ad-disable-advice 'evil-paste-after 'around 't/advice-paste-indent)
;; (ad-activate 'evil-paste-after)

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
        (if (called-interactively-p t)
            (message "/%s" (mapconcat 'identity path "/"))
          (format "/%s" (mapconcat 'identity path "/")))))))

(defun t/set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly 🚀"
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

(defun t/split-window-sensibly (&optional window)
  (cond
   ((and (> (window-width window)
            (* 2 (window-height window)))
         (window-splittable-p window 'horizontal))
    (with-selected-window window
      (split-window-right)))
   ((window-splittable-p window)
    (with-selected-window window
      (split-window-below)))))

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
   t ;; keep last modified time
   t ;; create parents
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

(defun t/move-line-up ()
  "Move the current line up one line. Preserves trailing commas."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (let ((handle-comma (and (t/prev-line-ends-in-comma)
                               (not (t/current-line-ends-in-comma)))))
        (transpose-lines 1)
        (when handle-comma
          (forward-line -2)
          (end-of-line)
          (insert ",")
          (forward-line)
          (end-of-line)
          (delete-char -1))))
    (forward-line -1)
    (move-to-column col)))

(defun t/move-line-down ()
  "Move the current line down one line. Preserves trailing commas"
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (let ((handle-comma (and (t/current-line-ends-in-comma)
                               (not (t/next-line-ends-in-comma)))))
        (when handle-comma
          (end-of-line)
          (delete-char -1))
        (forward-line)
        (transpose-lines 1)
        (when handle-comma
          (forward-line -2)
          (end-of-line)
          (insert ","))))
    (forward-line)
    (move-to-column col)))

(defun t/find-org-files-recursively (&optional directory filext)
  "Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead."
  (interactive "DDirectory: ")
  (let* (org-file-list
         (case-fold-search t)         ; filesystems are case sensitive
         (file-name-regex "^[^.#].*") ; exclude dot, autosave, and backup files
         (filext (or filext "org$\\\|org_archive"))
         (fileregex (format "%s\\.\\(%s$\\)" file-name-regex filext))
         (cur-dir-list (directory-files directory t file-name-regex)))
    ;; loop over directory listing
    (dolist (file-or-dir cur-dir-list org-file-list) ; returns org-file-list
      (cond
       ((file-regular-p file-or-dir) ; regular files
        (if (string-match fileregex file-or-dir) ; org files
            (add-to-list 'org-file-list file-or-dir)))
       ((file-directory-p file-or-dir)
        (dolist (org-file (t/find-org-files-recursively file-or-dir filext)
                          org-file-list) ; add files found to result
          (add-to-list 'org-file-list org-file)))))))

(defun t/org-fix-inline-images ()
  "Fix redisplaying images after executing org babel code."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(provide 't-defuns)
