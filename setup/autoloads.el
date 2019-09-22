;;; autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "t-cygwin" "t-cygwin.el" (0 0 0 0))
;;; Generated autoloads from t-cygwin.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-cygwin" '("t-cygwin/")))

;;;***

;;;### (autoloads nil "t-debug" "t-debug.el" (0 0 0 0))
;;; Generated autoloads from t-debug.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-debug" '("t-")))

;;;***

;;;### (autoloads nil "t-desktop" "t-desktop.el" (0 0 0 0))
;;; Generated autoloads from t-desktop.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-desktop" '("t/desktop-")))

;;;***

;;;### (autoloads nil "t-editor" "t-editor.el" (0 0 0 0))
;;; Generated autoloads from t-editor.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-editor" '("t-editor/config" "t/")))

;;;***

;;;### (autoloads nil "t-evil" "t-evil.el" (0 0 0 0))
;;; Generated autoloads from t-evil.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-evil" '("t-evil")))

;;;***

;;;### (autoloads nil "t-keys" "t-keys.el" (0 0 0 0))
;;; Generated autoloads from t-keys.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-keys" '("t-keys/")))

;;;***

;;;### (autoloads nil "t-linux" "t-linux.el" (0 0 0 0))
;;; Generated autoloads from t-linux.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-linux" '("t-linux/init")))

;;;***

;;;### (autoloads nil "t-load-theme" "t-load-theme.el" (0 0 0 0))
;;; Generated autoloads from t-load-theme.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-load-theme" '("t-load-theme/config")))

;;;***

;;;### (autoloads nil "t-mac" "t-mac.el" (0 0 0 0))
;;; Generated autoloads from t-mac.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-mac" '("t-mac/")))

;;;***

;;;### (autoloads nil "t-macros" "t-macros.el" (0 0 0 0))
;;; Generated autoloads from t-macros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-macros" '("comment" "t/")))

;;;***

;;;### (autoloads nil "t-modeline" "t-modeline.el" (0 0 0 0))
;;; Generated autoloads from t-modeline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-modeline" '("t-modeline/config")))

;;;***

;;;### (autoloads nil "t-org" "t-org.el" (0 0 0 0))
;;; Generated autoloads from t-org.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-org" '("lat-trh" "lon-trh" "t-" "t/")))

;;;***

;;;### (autoloads nil "t-packaging" "t-packaging.el" (0 0 0 0))
;;; Generated autoloads from t-packaging.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-packaging" '("use-package-verbose")))

;;;***

;;;### (autoloads nil "t-sane-defaults" "t-sane-defaults.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from t-sane-defaults.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-sane-defaults" '("t-sane-defaults/init" "t/init-mouse")))

;;;***

;;;### (autoloads nil "t-shell" "t-shell.el" (0 0 0 0))
;;; Generated autoloads from t-shell.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-shell" '("t-shell/config")))

;;;***

;;;### (autoloads nil "t-typography" "t-typography.el" (0 0 0 0))
;;; Generated autoloads from t-typography.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-typography" '("t-typography/config")))

;;;***

;;;### (autoloads nil "t-vc" "t-vc.el" (0 0 0 0))
;;; Generated autoloads from t-vc.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-vc" '("t-vc/config")))

;;;***

;;;### (autoloads nil nil ("t-calendar.el" "t-lang-graphql.el" "t-site-lisp.el"
;;;;;;  "t-text-objects.el" "t-which-key.el") (0 0 0 0))

;;;***

;;;### (autoloads nil "t-defuns" "t-defuns.el" (0 0 0 0))
;;; Generated autoloads from t-defuns.el

(autoload 't/exec-org-block "t-defuns" "\


\(fn NAME VARS)" nil nil)

(autoload 't/async-shell-command "t-defuns" "\
Execute `CMD' async, call `FN' with the result string.

\(fn NAME CMD &optional FN)" nil nil)

(autoload 't/evil-ex-define-cmd-local "t-defuns" "\
Locally binds the function FUNCTION to the command CMD.

\(fn CMD FUNCTION)" nil nil)

(autoload 't/send-buffer-to-scala-repl "t-defuns" "\
Send buffer to ensime repl, starts it if its not running" t nil)

(autoload 't/send-region-to-scala-repl "t-defuns" "\
Send region to ensime repl, starts it if its not running

\(fn START END)" t nil)

(autoload 't/clean-mode-line "t-defuns" nil t nil)

(autoload 't/css-kill-value "t-defuns" "\
kills the attribute of a css property" t nil)

(autoload 't/json-format "t-defuns" "\
pretty prints json in selected region" t nil)

(autoload 't/build-tags "t-defuns" "\
Build ctags file for projectile project, calls load-tags when done.

Effectively runs this for the current git project, e.g:

ctags -e -R --options=~/.emacs.d/ctags -f ~/.emacs.d/TAGS ~/.emacs.d/

The same can be done for the current folder only to place a TAGS file in it:

ctags -e -R .

Remember to build emacs --without-ctags and use the one from `brew' instead,
it's the one with the correct options needed to generate ctags that emacs
understands." t nil)

(autoload 't/load-tags "t-defuns" "\
loads project tags into tag table

\(fn TAGS)" nil nil)

(autoload 't/find-tag-at-point "t-defuns" "\
goes to tag at point, builds and/or loads project TAGS file first" t nil)

(autoload 't/copy-to-clipboard "t-defuns" "\
Copy text to os clipboard. Cygwin uses cygutils-extra's `putclip`. Mac uses builtin pbcopy.

\(fn TEXT &optional PUSH)" nil nil)

(autoload 't/open-in-desktop "t-defuns" "\
Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-06-12" t nil)

(autoload 't/open-line-above "t-defuns" "\
Insert a newline above the current line and put point at beginning." t nil)

(autoload 't/hippie-expand-no-case-fold "t-defuns" nil t nil)

(autoload 't/hippie-expand-lines "t-defuns" nil t nil)

(autoload 't/kill-other-buffers "t-defuns" "\
Kill all other buffers." t nil)

(autoload 't/rename-current-buffer-file "t-defuns" "\
Renames current buffer and file it is visiting." t nil)

(autoload 't/delete-current-buffer-file "t-defuns" "\
Removes file connected to current buffer and kills buffer." t nil)

(autoload 't/paredit-wrap-round-from-behind "t-defuns" nil t nil)

(autoload 't/untabify-buffer "t-defuns" "\
Remove tabs in buffer" t nil)

(autoload 't/indent-buffer "t-defuns" "\
Correctly indents a buffer" t nil)

(autoload 't/cleanup-buffer-whitespace-and-indent "t-defuns" "\
Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save." t nil)

(autoload 't/eval-region-or-last-sexp "t-defuns" nil t nil)

(autoload 't/eval-and-replace "t-defuns" "\
Evaluate and replace the preceding sexp with its value." t nil)

(autoload 't/lorem "t-defuns" "\
Insert a lorem ipsum." t nil)

(autoload 't/smart-beginning-of-line "t-defuns" "\
Move point to first non-whitespace character or beginning-of-line." t nil)

(autoload 't/delete-frame-or-hide-last-remaining-frame "t-defuns" "\
Delete the selected frame. If the last one, hide it instead." t nil)

(autoload 't/copy-buffer-file-name "t-defuns" nil t nil)

(autoload 't/copy-buffer-file-path "t-defuns" nil t nil)

(autoload 't/previous-window "t-defuns" "\
Skip back to previous window" t nil)

(autoload 't/buffer-mode "t-defuns" "\
Returns the major mode associated with a buffer.

\(fn BUFFER-OR-STRING)" nil nil)

(autoload 't/cursors-direction-is-up "t-defuns" "\
Returns t if the current cursor movement direction is 'up." nil nil)

(autoload 't/cursors-direction-is-down "t-defuns" "\
Returns t if the current cursor movement direction is 'down." nil nil)

(autoload 't/split-window-right-and-move-there-dammit "t-defuns" nil t nil)

(autoload 't/split-window-below-and-move-there-dammit "t-defuns" nil t nil)

(autoload 't/config-reload "t-defuns" nil t nil)

(autoload 't/face-color-b "t-defuns" "\
Get `:background' color of `attr'

\(fn ATTR)" nil nil)

(autoload 't/face-color-f "t-defuns" "\
Get `:foreground' color of `attr'

\(fn ATTR)" nil nil)

(autoload 't/switch-theme "t-defuns" "\
Switch theme, disabling previously loaded

\(fn THEME)" t nil)

(autoload 't/toggle-theme-dark-light "t-defuns" "\
Toggles between themes `doom-vibrant' and `spacemacs-light'" t nil)

(autoload 't/reload-font "t-defuns" "\
Reload font to make e.g. font size changes have effect." t nil)

(autoload 't/fix-fira-ligatures "t-defuns" nil t nil)

(autoload 't/decrease-font-size "t-defuns" nil t nil)

(autoload 't/increase-font-size "t-defuns" nil t nil)

(autoload 't/reset-font-size "t-defuns" "\


\(fn &optional ARGS)" t nil)

(autoload 'make-orgcapture-frame "t-defuns" "\
@torgeir: credits https://github.com/jjasghar/alfred-org-capture/blob/master/el/alfred-org-capture.el
  Create a new frame and run org-capture." t nil)

(autoload 't/sudo-edit "t-defuns" "\
Edit currently visited file as root.

   With a prefix ARG prompt for a file to visit.
   Will also prompt for a file to visit if current
   buffer is not visiting a file.

\(fn &optional ARG)" t nil)

(autoload 'server-remove-kill-buffer-hook "t-defuns" nil nil nil)

(autoload 't/prefix-arg-universal\? "t-defuns" nil nil nil)

(autoload 't/indent-after-paste "t-defuns" "\


\(fn FN &rest ARGS)" nil nil)

(autoload 't/comint-clear-buffer "t-defuns" nil t nil)

(autoload 't/uniquify-lines "t-defuns" "\
Remove duplicate adjacent lines in region or current buffer" t nil)

(autoload 't/sort-lines "t-defuns" "\
Sort lines in region or current buffer" t nil)

(autoload 't/switch-to-previous-buffer "t-defuns" "\
Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers." t nil)

(autoload 't/switch-to-scratch-buffer "t-defuns" "\
Switch to the `*scratch*' buffer. Create it first if needed." t nil)

(autoload 't/shorten-directory "t-defuns" "\
Show up to `max-length' characters of a directory name `dir'.

\(fn DIR MAX-LENGTH)" nil nil)

(autoload 't/find-xml-path "t-defuns" "\
Display the hierarchy of XML elements the point is on as a path." t nil)

(autoload 't/set-emoji-font "t-defuns" "\
Adjust the font settings of FRAME so Emacs can display emoji properly 🚀

\(fn FRAME)" nil nil)

(autoload 't/split-window-sensibly "t-defuns" "\


\(fn &optional WINDOW)" nil nil)

(autoload 't/date-time "t-defuns" "\
Insert current date-time string in full ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00" nil nil)

(autoload 't/date-time-for-filename "t-defuns" "\
Return date-time iso 8601 string suitable for filename" nil nil)

(autoload 't/elpa-backup-directory "t-defuns" "\
Returns the directory name of an elpa backup that would run now." nil nil)

(autoload 't/elpa-backup "t-defuns" "\
Backup the current elpa folder to elpa-backups." t nil)

(autoload 't/upgrade-packages "t-defuns" "\
Upgrade packages after backing up the current elpa files." t nil)

(autoload 't/current-line-ends-in-comma "t-defuns" "\
Return whether the current line is suffixed with ','" nil nil)

(autoload 't/prev-line-ends-in-comma "t-defuns" "\
Return whether the current line is suffixed with ','" nil nil)

(autoload 't/next-line-ends-in-comma "t-defuns" "\
Return whether the current line is suffixed with ','" nil nil)

(autoload 't/move-line-up "t-defuns" "\
Move the current line(s) down one line.

\(fn ARG)" t nil)

(autoload 't/move-line-down "t-defuns" "\
Move the current line(s) down one line.

\(fn ARG)" t nil)

(autoload 't/find-org-files-recursively "t-defuns" "\
Return .org and .org_archive files recursively from DIRECTORY.
If FILEXT is provided, return files with extension FILEXT instead.

\(fn &optional DIRECTORY FILEXT)" t nil)

(autoload 't/org-fix-inline-images "t-defuns" "\
Fix redisplaying images after executing org babel code." nil nil)

(autoload 't/project-root "t-defuns" "\
Get project root without throwing" nil nil)

(autoload 't/volatile-kill-buffer "t-defuns" "\
Kill current buffer unconditionally." t nil)

(autoload 't/volatile-kill-buffer-and-window "t-defuns" "\
Kill current buffer and the window unconditionally." t nil)

(autoload 't/grab-chrome-url "t-defuns" "\
Grab the frontmost url out of chrome using `org-mac-grab-link'" t nil)

(autoload 't/browse-chrome-url-in-w3m "t-defuns" "\
Open the frontmost chrome url in `w3m'. " t nil)

(autoload 't/get-url "t-defuns" "\
Get url synchronously.

\(fn URL)" nil nil)

(autoload 't/fetch "t-defuns" "\
Insert url contents in current buffer. Drops headers and 2x empty lines before content.

\(fn URL)" t nil)

(autoload 't/fetch-chrome-url "t-defuns" "\
Insert contents of frontmost url of chrome in buffer." t nil)

(autoload 't/browse-url-at-point "t-defuns" nil t nil)

(autoload 't/last-weekday-of-month-p "t-defuns" nil nil nil)

(autoload 't/face-at-point "t-defuns" "\
Echo the face at point.

\(fn POS)" t nil)

(autoload 't/font-lock-test-faces "t-defuns" "\
Outputs test strings with all font lock faces to show colors." t nil)

(autoload 't/find-file-check-make-large-file-read-only-hook "t-defuns" "\
If a file is over a given size, make the buffer read only." nil nil)

(autoload 't/run-osascript "t-defuns" "\
Run applescript.

\(fn S)" nil nil)

(autoload 't/osascript-activate "t-defuns" "\
Run applescript to activate application.

\(fn APP)" nil nil)

(autoload 't/osascript-show-url "t-defuns" "\


\(fn URL)" nil nil)

(autoload 't/open-in-intellij "t-defuns" "\
Opens current file in IntelliJ IDEA." t nil)

(autoload 't/propertize-read-only "t-defuns" "\


\(fn STR)" nil nil)

(autoload 't/strip-text-properties "t-defuns" "\


\(fn TXT)" nil nil)

(autoload 't/mobile-inbox-count "t-defuns" "\
Counts the number of items in `org-mobile-inbox-for-pull'." nil nil)

(autoload 't/get-string-from-file "t-defuns" "\
Return `file-path's file content.

\(fn FILE-PATH)" nil nil)

(autoload 't/re-seq "t-defuns" "\
Get a list of all regexp matches (from the first group) in a string

\(fn REGEXP STRING)" nil nil)

(autoload 't/re-seq-in-file "t-defuns" "\


\(fn REGEX FILE)" nil nil)

(autoload 't/fn-prefix "t-defuns" "\


\(fn PREFIX MODULE)" nil nil)

(autoload 't/call-fns "t-defuns" "\


\(fn FNS)" nil nil)

(autoload 't/call-prefix "t-defuns" "\


\(fn PREFIX MODULE)" nil nil)

(autoload 't/call-init "t-defuns" "\


\(fn PREFIX-FMT PKG)" nil nil)

(autoload 't/buffer-finished-p "t-defuns" "\
Return non-nil value if the buffer has an ended process.

\(fn B)" nil nil)

(autoload 't/term-quit-if-finished "t-defuns" "\


\(fn &optional ELSE)" t nil)

(autoload 't/term-kill-if-finished "t-defuns" "\


\(fn &optional ELSE)" t nil)

(autoload 't/neotree-open-file "t-defuns" nil t nil)

(autoload 't/backward-down-sexp "t-defuns" "\
Move backward to the start of the previous sexp." t nil)

(autoload 't/forward-down-sexp "t-defuns" "\
Move forward to the end of the previous sexp." t nil)

(autoload 't/forward-sexp "t-defuns" nil t nil)

(autoload 't/backward-sexp "t-defuns" nil t nil)

(autoload 't/helm-files-emacs-init-files "t-defuns" nil t nil)

(autoload 't/newline-expand-braces "t-defuns" "\
Newline like `evil-ret', but expand (), [] and {} with newline in between, and indent accordingly." t nil)

(autoload 't/recompile-elpa "t-defuns" "\
Recompile the elpa/ directory to resolve byte compilation issues." t nil)

(autoload 't/describe "t-defuns" "\
Describe functions, features, symbols, or run help-apropos if it's not found.

\(fn &optional SAT)" t nil)

(autoload 't/unbind "t-defuns" "\
Unbind function or symbol depending on type.

\(fn FN-OR-S)" t nil)

(autoload 't/add-to-list "t-defuns" "\
Adds items to the list `l'.

\(fn L ITEM-OR-ITEMS)" nil nil)

(autoload 't/toggle-line-numbers "t-defuns" "\
Toggle line numbers on or off." t nil)

(autoload 't/toggle-relative-line-numbers "t-defuns" "\
Toggle relative line numbers on or off." t nil)

(autoload 't/highlight-logging "t-defuns" "\
Add log highlighting to current major mode." t nil)

(autoload 't/clone "t-defuns" "\
Clone a github repo to `~/Code/<repo-name>'.

\(fn REPO)" t nil)

(autoload 't/set-company-backends "t-defuns" "\
Set company backends.

\(fn &rest BACKENDS)" nil nil)

(autoload 't/add-company-backends "t-defuns" "\
Add list of grouped company backends.

\(fn &rest BACKENDS)" nil nil)

(autoload 't/add-company-backends-hook "t-defuns" "\
Add list of grouped company backends for `mode-hook'.

\(fn MODE-HOOK &rest BACKENDS)" nil nil)

(autoload 't/visit-git-link-pulls "t-defuns" "\
Navigate to /pulls for the current git repo." t nil)

(autoload 't/projectile-dired "t-defuns" nil t nil)

(autoload 't/projectile-desktop "t-defuns" nil t nil)

(autoload 't/projectile-magit-status "t-defuns" nil t nil)

(autoload 't/projectile-helm-ag "t-defuns" nil t nil)

(autoload 't/projectile-visit-git-link-pulls "t-defuns" nil t nil)

(autoload 't/margins-global "t-defuns" "\
Set global frame margins.

\(fn L &optional R)" t nil)

(autoload 't/margins-local "t-defuns" "\
Set buffer local frame margin.

\(fn L &optional R)" t nil)

(autoload 't/eww-toggle-images "t-defuns" "\
Toggle whether images are loaded and reload the current page fro cache." t nil)

(autoload 't/last-used-window-buffer "t-defuns" "\
Switch to the window that displays the most recently selected buffer." t nil)

(autoload 't/safe-restart-emacs "t-defuns" "\
Restart emacs if config parses correctly." t nil)

(autoload 't/word-at-point "t-defuns" "\
Returns word under cursor." nil nil)

(autoload 't/ip "t-defuns" nil t nil)

(autoload 't/transparency "t-defuns" "\
Sets the transparency of the frame window. 0=transparent/100=opaque

\(fn VALUE)" t nil)

(autoload 't/clone-github-repo-from-chrome "t-defuns" "\
Clones the github repo of the currently visisble chrome tab." t nil)

(autoload 't/ediff-use-both "t-defuns" nil t nil)

(autoload 't/lookup-key "t-defuns" "\
Search for KEY in all known keymaps.

\(fn KEY)" nil nil)

(autoload 't/lookup-key-prefix "t-defuns" "\
Search for KEY as prefix in all known keymaps.

\(fn KEY)" nil nil)

(autoload 't/fns "t-defuns" "\
List functions." nil nil)

(autoload 't/interactive-fns "t-defuns" "\
List interactive functions." nil nil)

(autoload 't/complete-with-helm-then "t-defuns" "\
Complete from list with helm, e.g. (t/complete-with-helm-then '(one two) 'insert)

\(fn LIST FN)" t nil)

(autoload 't/locally-disable-cursor "t-defuns" "\
Locally disable cursor in buffer." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "t-defuns" '("*t-adjusted-font-size*" "t/")))

;;;***

(provide 'autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; autoloads.el ends here
