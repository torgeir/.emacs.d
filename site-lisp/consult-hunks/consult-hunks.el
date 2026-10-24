;;; consult-hunks.el --- Browse, stage, unstage and kill git hunks -*- lexical-binding: t; -*-

;; Author: @torgeir
;; Version: 1.0.0
;; Keywords: consult git hunks vc
;; Package-Requires: ((emacs "28.1") (consult "3.0"))

;;; Commentary:

;; A consult interface for git hunks - browsing, staging, unstaging and
;; killing.  A re-implementation of `helm-hunks' on top of consult.
;;
;; Run `consult-hunks' and jump around the unstaged hunks of the project,
;; the hunk under the cursor is previewed where it lives.  Stage it with
;; `C-s', kill it with `C-k'.
;;
;; Run `consult-hunks-current-buffer' to jump around the current buffer
;; only.
;;
;; Run `consult-hunks-staged' to jump around staged hunks, unstage with
;; `C-u', and `consult-hunks-staged-current-buffer' for the current
;; buffer only.
;;
;; Every candidate carries its diff, collapsed onto the candidate line
;; by vertico, so the added and deleted text can be narrowed on.  `C-c
;; C-p' cycles what the preview shows: the hunk where it lives, the same
;; diff inline after the line the hunk starts at, and the file as git
;; has it side by side with the file the hunk changes it into, both
;; scrolled to the hunk - the way ediff shows a change, without ediff.
;; Jump to a hunk in "other window" or "other frame" with `C-c o' and
;; `C-c C-o'.
;;
;; Commit with `C-c C-c', amend with `C-c C-a', quit with `C-c C-k'.
;;
;; Leave a note above a hunk with `C-c C-f': it is prompted for and
;; written right above the change, in the comment syntax of the file,
;; as `FIXME: what you typed' - a note for you, or for the AI you hand
;; the file to, to run into next to the change it is about.  A second
;; note on the same change lands below the first.
;;
;; Evil users get plain keys in normal state as well: `j' and `k' to
;; move, `s' to stage, `u' to unstage and `c' to leave a note.  The
;; rest of normal state is left alone, `p' still pastes.
;;
;; Staging, unstaging and killing act on the selected hunk and then
;; restart the session on the refreshed list of hunks, so hunks can be
;; staged one after the other.
;;
;; Credits/inspiration: helm-hunks - https://github.com/torgeir/helm-hunks.el

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'diff-mode)
(require 'newcomment)
(require 'subr-x)

(declare-function vertico--update "ext:vertico" (&optional interruptible))
(declare-function vertico--goto "ext:vertico" (index))
(declare-function vertico--exhibit "ext:vertico" ())
(defvar vertico--candidates)

(defgroup consult-hunks nil
  "Browse, stage, unstage and kill git hunks."
  :group 'consult)

(defcustom consult-hunks-commit-fn #'magit-commit
  "Command called interactively to commit."
  :type 'function)

(defcustom consult-hunks-commit-amend-fn #'magit-commit-amend
  "Command called interactively to amend."
  :type 'function)

(defcustom consult-hunks-refresh-hook '(consult-hunks-refresh-diff-hl)
  "Hooks run in the buffer visiting a hunk after it changed in git.
Use it to refresh your favorite git gutter."
  :type 'hook)

(defcustom consult-hunks-show-diffs nil
  "Show the diff of the previewed hunk inline, where the hunk lives.
Cycled by `consult-hunks-cycle-diffs' while the session is open.
The candidates always carry their diff, but vertico collapses it onto
the candidate line, so it reads as one line and is there to narrow on."
  :type 'boolean)

(defcustom consult-hunks-show-split nil
  "Show the previewed hunk with its old and its new version side by side.
Cycled by `consult-hunks-cycle-diffs' while the session is open.  One
window shows the file as git has it - the index for an unstaged hunk,
HEAD for a staged one - the other the version the hunk changes it
into, both scrolled to the hunk."
  :type 'boolean)

(defcustom consult-hunks-comment-tag "FIXME"
  "Tag the notes of `consult-hunks-comment' start with.
A note reads like `FIXME: what to do about this\=', in the comment
syntax of the file, right above the change - where you, or the AI you
hand the file to, run into it next to what it is about."
  :type 'string)

(defcustom consult-hunks-split-side 'right
  "Side the new version is shown on in the side by side view."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Below" below)))

(defconst consult-hunks--hunk-re
  "\\`@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regex matching a git diff hunk header, `@@ -del,del-len +add,add-len @@'.")

(defvar consult-hunks--history nil
  "History of `consult-hunks'.")

(defvar consult-hunks--root nil
  "Root of the repository of the running session.")

(defvar consult-hunks--staged nil
  "Non-nil when the running session shows staged hunks.")

(defvar consult-hunks--restart nil
  "Function restarting the running session.")

(defvar consult-hunks--session-hunks nil
  "Hunks the running session was started on.")

(defvar consult-hunks--entry-state nil
  "Evil state the restarted session is entered in.")

(defun consult-hunks-refresh-diff-hl ()
  "Refresh `diff-hl-mode' in the current buffer, if it is on."
  (when (and (bound-and-true-p diff-hl-mode) (fboundp 'diff-hl-update))
    (diff-hl-update)))

;;;; Git

(defun consult-hunks--find-root ()
  "Return the root of the git repository of `default-directory'.
Asked of git itself, as `vc-root-dir' only answers in buffers visiting
a file."
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
      (user-error "Not inside a git repository"))
    (file-name-as-directory (string-trim (buffer-string)))))

(defun consult-hunks--git (&rest args)
  "Run git with ARGS in the repository root and return its output."
  (let ((default-directory consult-hunks--root))
    (with-temp-buffer
      (unless (zerop (apply #'process-file "git" nil t nil args))
        (user-error "git %s: %s" (car args) (string-trim (buffer-string))))
      (buffer-string))))

(defun consult-hunks--git-apply (hunk &rest args)
  "Pipe the patch of HUNK to `git apply' with ARGS."
  (let ((default-directory consult-hunks--root))
    (with-temp-buffer
      (insert (alist-get 'patch hunk))
      (unless (zerop (apply #'call-process-region (point-min) (point-max)
                            "git" 'delete t nil
                            (append '("apply" "--unidiff-zero") args '("-"))))
        (user-error "git apply: %s" (string-trim (buffer-string)))))))

(defun consult-hunks--diff (file)
  "Return the unified diff of the session, restricted to FILE when non-nil."
  (apply #'consult-hunks--git
         `("-c" "core.quotepath=false" "--no-pager" "diff"
           "--no-color" "--no-ext-diff" "--unified=0"
           ,@(and consult-hunks--staged '("--staged"))
           ,@(and file (list "--" file)))))

;;;; Parsing

(defun consult-hunks--make-hunk (header lines file)
  "Build a hunk from its HEADER lines, its own LINES and its FILE.
A hunk is an alist of the `file' it changes, the `line' the change
starts at and the `old-line' it started at before, how many lines it
adds and deletes, the `type' of change, the `content' of the diff and
the `patch' that applies it on its own."
  (let ((hunk-header (car lines))
        ;; with --unified=0 every content line is an addition, a deletion
        ;; or the no-newline marker, which drops the trailing empty line
        (content (seq-filter (lambda (line) (string-match-p "\\`[-+\\]" line))
                             (cdr lines))))
    (when (string-match consult-hunks--hunk-re hunk-header)
      (let* ((del-line (string-to-number (match-string 1 hunk-header)))
             (del-len (string-to-number (or (match-string 2 hunk-header) "1")))
             (add-line (string-to-number (match-string 3 hunk-header)))
             (add-len (string-to-number (or (match-string 4 hunk-header) "1")))
             (type (cond ((zerop del-len) 'added)
                         ((zerop add-len) 'deleted)
                         (t 'modified))))
        `((file . ,file)
          ;; a deleted hunk sits between add-line and the line after it,
          ;; an added one between del-line and the line after it
          (line . ,(if (zerop add-len) (1+ add-line) add-line))
          (old-line . ,(if (zerop del-len) (1+ del-line) del-line))
          (add-len . ,add-len)
          (del-len . ,del-len)
          (type . ,type)
          (content . ,(string-join content "\n"))
          (patch . ,(concat (string-join (reverse header) "\n") "\n"
                            hunk-header "\n"
                            (string-join content "\n") "\n")))))))

(defun consult-hunks--parse (diff)
  "Parse the unified DIFF into a list of hunks."
  (let (hunks header file lines)
    (cl-flet ((collect ()
                ;; the lines of the hunk are collected in reverse
                (when-let* ((lines (reverse lines))
                            (hunk (consult-hunks--make-hunk header lines file)))
                  (push hunk hunks))
                (setq lines nil)))
      (dolist (line (split-string diff "\n"))
        (cond
         ((string-prefix-p "diff --git " line)
          (collect)
          (setq header (list line) file nil))
         ((string-match "\\`--- a/\\(.*\\)" line)
          (push line header)
          (setq file (match-string 1 line)))
         ((string-match "\\`\\+\\+\\+ b/\\(.*\\)" line)
          (push line header)
          ;; the b/ side is the file on disk, renames included
          (setq file (match-string 1 line)))
         ((string-match-p consult-hunks--hunk-re line)
          (collect)
          (setq lines (list line)))
         (lines (push line lines))
         (header (push line header))))
      (collect))
    (nreverse hunks)))

(defun consult-hunks--hunks (file)
  "Return the hunks of the session, restricted to FILE when non-nil."
  (consult-hunks--parse (consult-hunks--diff file)))

;;;; Candidates

(defun consult-hunks--fontify-diff (content)
  "Fontify CONTENT as a diff, the way `diff-mode' shows it."
  (with-temp-buffer
    ;; the diff-mode font lock keywords match the newline ending each
    ;; line, so the last line stays unfontified without one
    (insert content "\n")
    (delay-mode-hooks (diff-mode))
    (font-lock-ensure)
    (buffer-substring (point-min) (1- (point-max)))))

(defun consult-hunks--candidate (hunk)
  "Format HUNK as a candidate string carrying the hunk itself."
  (let* ((file (alist-get 'file hunk))
         (line (number-to-string (alist-get 'line hunk)))
         (type (alist-get 'type hunk))
         (content (alist-get 'content hunk))
         (candidate (concat file ":" line " ("
                            (propertize (symbol-name type) 'face
                                        (pcase type
                                          ('added 'diff-added)
                                          ('deleted 'diff-removed)
                                          (_ 'diff-changed)))
                            ")"
                            ;; vertico collapses the diff onto the
                            ;; candidate line, which keeps the added and
                            ;; deleted text available to narrow on
                            (unless (string-empty-p content)
                              (concat "\n" (consult-hunks--fontify-diff content))))))
    (add-text-properties 0 (length file)
                         `(face consult-file consult--prefix-group ,file)
                         candidate)
    (put-text-property (1+ (length file)) (+ 1 (length file) (length line))
                       'face 'consult-line-number candidate)
    (put-text-property 0 1 'consult--candidate hunk candidate)
    candidate))

;;;; Preview and jumping

(defun consult-hunks--position (hunk &optional find-file)
  "Return the position marker of HUNK.
FIND-FILE is the file open function, defaulting to `consult--file-action'."
  (when hunk
    (when-let* ((buffer (funcall (or find-file #'consult--file-action)
                                 (expand-file-name (alist-get 'file hunk)
                                                   consult-hunks--root))))
      (consult--marker-from-line-column buffer (alist-get 'line hunk) 0))))

(defvar consult-hunks--diff-overlay nil
  "Overlay showing the diff of the previewed hunk.")

(defvar consult-hunks--open nil
  "Function of the running session opening files for preview.")

(defun consult-hunks--hide-diff ()
  "Remove the inline diff of the previewed hunk."
  (when consult-hunks--diff-overlay
    (delete-overlay consult-hunks--diff-overlay)
    (setq consult-hunks--diff-overlay nil)))

(defun consult-hunks--show-diff (pos hunk)
  "Show the diff of HUNK inline, after the line at POS."
  (when-let* ((content (alist-get 'content hunk))
              ((not (string-empty-p content)))
              (buffer (if (markerp pos) (marker-buffer pos) (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (goto-char pos)
        (let ((overlay (make-overlay (line-end-position) (line-end-position))))
          ;; an empty overlay, the diff is virtual text pushing the rest
          ;; of the buffer down for as long as the hunk is previewed
          (overlay-put overlay 'after-string
                       (concat "\n" (consult-hunks--fontify-diff content)))
          (setq consult-hunks--diff-overlay overlay))))))

(defvar consult-hunks--preview nil
  "Function of the running session previewing a hunk.")

(defun consult-hunks--preview-hunk (action hunk open jump)
  "Preview HUNK, or jump to it for good when ACTION is `return'.
OPEN opens the file of a preview, JUMP is the jump state of the
session.  What a preview looks like is up to `consult-hunks-show-diffs'
and `consult-hunks-show-split'."
  (consult-hunks--hide-diff)
  (consult-hunks--hide-split)
  (if (and (eq action 'preview) hunk consult-hunks-show-split)
      (consult-hunks--preview-split hunk open jump)
    (when (eq action 'return) (consult-hunks--restore-wconf))
    (let ((pos (consult-hunks--position
                hunk (and (not (eq action 'return)) open))))
      (funcall jump action pos)
      (when (and (eq action 'preview) pos consult-hunks-show-diffs)
        (consult-hunks--show-diff pos hunk)))))

(defun consult-hunks--state ()
  "State function previewing the hunk where it lives."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (setq consult-hunks--open open
          consult-hunks--preview
          (lambda (hunk) (consult-hunks--preview-hunk 'preview hunk open jump)))
    (lambda (action hunk)
      (unless hunk (funcall open))
      (when (eq action 'return)
        (setq consult-hunks--open nil
              consult-hunks--preview nil))
      (consult-hunks--preview-hunk action hunk open jump))))

;;;; The old and the new, side by side

(defface consult-hunks-old '((t :inherit diff-removed))
  "Face of the lines a hunk deletes, in the window of the old version.")

(defface consult-hunks-new '((t :inherit diff-added))
  "Face of the lines a hunk adds, in the window of the new version.")

(defconst consult-hunks--blob-name " *consult-hunks "
  "Prefix of the name of a buffer holding a file the way git has it.
The leading space keeps them out of the buffer list, they live as long
as the session does.")

(defvar consult-hunks--blob-buffers nil
  "Alist of a (REV . FILE) and the buffer holding that version of FILE.")

(defvar consult-hunks--wconf nil
  "Window configuration from before the running session.")

(defvar consult-hunks--split nil
  "Windows of the side by side view, the old version in the car.")

(defvar consult-hunks--split-built nil
  "Non-nil once the session has built the side by side view.")

(defvar consult-hunks--split-overlays nil
  "Overlays highlighting the hunk in the side by side view.")

(defun consult-hunks--blob (rev file)
  "Return the contents of FILE at REV, nil when git has no such blob.
REV is a git revision, nil stands for the index."
  (let ((default-directory consult-hunks--root))
    (with-temp-buffer
      (and (zerop (process-file "git" nil t nil "cat-file" "blob"
                                (concat rev ":" file)))
           (buffer-string)))))

(defun consult-hunks--blob-buffer (rev file)
  "Return a read-only buffer holding FILE the way it is at REV.
REV is a git revision, nil stands for the index and `gone' for a file
that is not there at all, as the new version of a deleted file is.
The buffers are cached for as long as the session runs, so moving
between the hunks of a file costs one call to git."
  (let ((key (cons rev file)))
    (or (cdr (assoc key consult-hunks--blob-buffers))
        (let ((buffer (generate-new-buffer
                       (format "%s%s:%s*" consult-hunks--blob-name
                               (cond ((eq rev 'gone) "gone") (rev) (t "index"))
                               file))))
          (with-current-buffer buffer
            (unless (eq rev 'gone)
              (insert (or (consult-hunks--blob rev file) "")))
            (goto-char (point-min))
            ;; the major mode is the one of the file, found from its
            ;; name, its hooks are left alone - this is a buffer to look
            ;; at, not one to work in
            (ignore-errors
              (let ((buffer-file-name (expand-file-name file consult-hunks--root)))
                (delay-mode-hooks (set-auto-mode))))
            ;; `font-lock-mode' refuses to turn on in a buffer named with
            ;; a leading space, as the buffers of the view are, so it is
            ;; turned on under a name without one
            (let ((name (buffer-name)))
              (rename-buffer (string-trim-left name) t)
              (unwind-protect (font-lock-mode 1)
                (rename-buffer name)))
            (setq buffer-read-only t)
            (set-buffer-modified-p nil))
          (push (cons key buffer) consult-hunks--blob-buffers)
          buffer))))

(defun consult-hunks--kill-blob-buffers ()
  "Kill the buffers holding files the way git has them.
Buffers left behind by a session that did not get to clean up go as
well, they are the ones named like them."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p consult-hunks--blob-name (buffer-name buffer))
      (kill-buffer buffer)))
  (setq consult-hunks--blob-buffers nil))

(defun consult-hunks--split-windows ()
  "Return the windows of the side by side view, the old version in the car.
The view is built when it is not up, on a frame of two windows - the
configuration from before the session is put back when it ends."
  (if (and (window-live-p (car consult-hunks--split))
           (window-live-p (cdr consult-hunks--split)))
      consult-hunks--split
    (let ((window (consult--original-window)))
      (ignore-errors (delete-other-windows window))
      (setq consult-hunks--split-built t
            consult-hunks--split
            (cons window (split-window window nil consult-hunks-split-side))))))

(defun consult-hunks--hide-split ()
  "Remove the highlights of the hunk of the side by side view."
  (mapc #'delete-overlay consult-hunks--split-overlays)
  (setq consult-hunks--split-overlays nil))

(defun consult-hunks--close-split ()
  "Take the side by side view down, leaving the window it was built in.
The configuration from before the session is only put back once the
minibuffer is gone, by `consult-hunks--restore-wconf'."
  (consult-hunks--hide-split)
  (let ((windows consult-hunks--split))
    (setq consult-hunks--split nil)
    (when (and (window-live-p (cdr windows))
               (not (eq (car windows) (cdr windows))))
      (ignore-errors (delete-window (cdr windows))))))

(defun consult-hunks--restore-wconf ()
  "Put the window configuration from before the session back.
Only for when the minibuffer is gone - the configuration was saved
without one, and restoring it while one is up would take it along."
  (consult-hunks--hide-split)
  (setq consult-hunks--split nil)
  (when (and consult-hunks--split-built
             (window-configuration-p consult-hunks--wconf))
    (setq consult-hunks--split-built nil)
    (set-window-configuration consult-hunks--wconf)))

(defun consult-hunks--show-at (window buffer line)
  "Show BUFFER in WINDOW, scrolled to LINE.
Where LINE ends up is left to `consult-after-jump-hook', the same hook
that scrolls the window of a jump, so both sides of the view line up."
  (unless (eq (window-buffer window) buffer)
    (set-window-buffer window buffer))
  (with-selected-window window
    (goto-char (point-min))
    (forward-line (1- line))
    (run-hooks 'consult-after-jump-hook)))

(defun consult-hunks--highlight (buffer line len face)
  "Highlight the LEN lines from LINE in BUFFER with FACE."
  (when (and (buffer-live-p buffer) (> len 0))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- line))
          (let ((overlay (make-overlay (point) (progn (forward-line len) (point)))))
            (overlay-put overlay 'face face)
            (push overlay consult-hunks--split-overlays)))))))

(defun consult-hunks--preview-split (hunk open jump)
  "Preview HUNK with the old and the new version of its file side by side.
OPEN opens the file of a preview, JUMP is the jump state of the
session.  The old version comes from git - the index when the session
browses unstaged hunks, HEAD when it browses staged ones.  The new
version of an unstaged hunk is the file on disk, shown through JUMP so
consult puts its point back when the session ends; the new version of
a staged one is the index, as the lines of a staged hunk are the lines
of the index, not of the file on disk."
  (let* ((file (alist-get 'file hunk))
         (line (alist-get 'line hunk))
         (old-line (alist-get 'old-line hunk))
         (windows (consult-hunks--split-windows))
         (new-window (cdr windows))
         (old (consult-hunks--blob-buffer
               (and consult-hunks--staged "HEAD") file))
         (pos (unless consult-hunks--staged
                (consult-hunks--position hunk open))))
    (consult-hunks--show-at (car windows) old old-line)
    (consult-hunks--highlight old old-line (alist-get 'del-len hunk)
                              'consult-hunks-old)
    (cond
     (consult-hunks--staged
      ;; nothing of the jump state is on screen, let it put back
      ;; whatever it previewed before
      (funcall jump 'preview nil)
      (consult-hunks--show-at new-window
                              (consult-hunks--blob-buffer nil file) line))
     (pos
      ;; the buffer goes in the window first, the jump state then finds
      ;; it there instead of displaying it wherever it pleases
      (set-window-buffer new-window (marker-buffer pos))
      (funcall jump 'preview pos))
     (t
      ;; the file is no longer on disk, the hunk deletes all of it
      (funcall jump 'preview nil)
      (consult-hunks--show-at new-window
                              (consult-hunks--blob-buffer 'gone file) 1)))
    (consult-hunks--highlight (window-buffer new-window) line
                              (alist-get 'add-len hunk) 'consult-hunks-new)))

;;;; Session commands

(defun consult-hunks--selection ()
  "Return the hunk selected in the minibuffer."
  (when-let* ((cand (run-hook-with-args-until-success
                     'consult--completion-candidate-hook)))
    (get-text-property 0 'consult--candidate cand)))

(defun consult-hunks--quit-and-run (fn)
  "Quit the session and run FN afterwards.
FN runs from a timer, where a `user-error' would be reported as a
failing timer, so it is shown as the message it is meant to be."
  (run-at-time 0 nil
               (lambda ()
                 (condition-case err (funcall fn)
                   (user-error (message "%s" (error-message-string err))))))
  (abort-recursive-edit))

(defun consult-hunks--restart-session (&optional preselect)
  "Quit and restart the session on the refreshed hunks.
PRESELECT is the hunk to come back to, a cons of a file and the line a
hunk of it starts at; without one the restarted session starts at the
first hunk.  The evil state is carried over, as staging from normal
state would otherwise land in the insert state every new minibuffer
starts in."
  (setq consult-hunks--entry-state (bound-and-true-p evil-state))
  ;; the restart runs from a timer, where the session and its dynamic
  ;; bindings are long gone, so take the function along
  (let ((restart consult-hunks--restart))
    (consult-hunks--quit-and-run (lambda () (funcall restart preselect)))))

(defun consult-hunks--where (hunk)
  "Return where HUNK stands in the session, to come back to it later.
That is its file, the line it starts at and its place in the list of
hunks - enough to find it again once the hunks have been refreshed,
whether it moved, grew or is gone altogether."
  (list (alist-get 'file hunk)
        (alist-get 'line hunk)
        (seq-position consult-hunks--session-hunks hunk #'eq)))

(defun consult-hunks--hunk-at (preselect hunks)
  "Return the hunk of HUNKS that PRESELECT points at.
PRESELECT is the file, the line and the place in the list of the hunk
the session stood on, as `consult-hunks--where' put them.  The first
hunk of that file at or after the line wins - a note written above a
change lands in the hunk that change ends up in, and staging a hunk
leaves the ones after it where they were.  Once the file is out of the
list, its last hunk having been staged, the hunk that took the place
in the list does: the list closes up under the selection, so staging
one hunk after the other never has to find its way back."
  (pcase-let ((`(,file ,line ,index) preselect))
    (or (seq-find (lambda (hunk) (and (equal file (alist-get 'file hunk))
                                      (>= (alist-get 'line hunk) line)))
                  hunks)
        (and index (nth (min index (1- (length hunks))) hunks)))))

(defun consult-hunks--select-hunk (preselect hunks candidates)
  "Move the selection to the hunk of PRESELECT, among HUNKS and CANDIDATES.
Only vertico is told where to go - it locks onto the candidate, so it
stays selected while the list is narrowed and is let go of once it no
longer matches.  Every other completion UI starts at the first
candidate, the way it always does."
  (when-let* ((preselect)
              ((bound-and-true-p vertico-mode))
              ((fboundp 'vertico--goto))
              (hunk (consult-hunks--hunk-at preselect hunks))
              (candidate (nth (seq-position hunks hunk #'eq) candidates)))
    ;; the candidates have to be computed before there is an index to
    ;; go to, and grouping reorders them, so the index is the one of
    ;; the candidate in vertico's own list
    (vertico--update)
    (when-let* ((index (seq-position vertico--candidates candidate)))
      (vertico--goto index)
      (vertico--exhibit))))

(defun consult-hunks--refresh (hunk &optional revert)
  "Refresh the buffer visiting the file of HUNK, reverting it when REVERT."
  (when-let* ((file (expand-file-name (alist-get 'file hunk) consult-hunks--root))
              (buffer (find-buffer-visiting file)))
    (with-current-buffer buffer
      (when (and revert (not (buffer-modified-p)))
        (revert-buffer t t t))
      (vc-refresh-state)
      (run-hooks 'consult-hunks-refresh-hook))))

(defun consult-hunks--act (fn &optional revert)
  "Run FN on the selected hunk, then restart on the refreshed hunks.
REVERT is passed to `consult-hunks--refresh'.  The session comes back
to the hunk that took the place of the one acted on, so the next one
is under the selection already."
  (when-let* ((hunk (consult-hunks--selection)))
    (let ((where (consult-hunks--where hunk)))
      (funcall fn hunk)
      (consult-hunks--refresh hunk revert)
      (consult-hunks--restart-session where))))

(defun consult-hunks-stage ()
  "Stage the selected hunk."
  (interactive)
  (unless consult-hunks--staged
    (consult-hunks--act (lambda (hunk) (consult-hunks--git-apply hunk "--cached")))))

(defun consult-hunks-unstage ()
  "Unstage the selected hunk."
  (interactive)
  (when consult-hunks--staged
    (consult-hunks--act (lambda (hunk)
                          (consult-hunks--git-apply hunk "--cached" "--reverse")))))

(defun consult-hunks-kill ()
  "Kill the selected hunk, reverting it in the file on disk."
  (interactive)
  (unless consult-hunks--staged
    (consult-hunks--act (lambda (hunk) (consult-hunks--git-apply hunk "--reverse"))
                        'revert)))

(defvar consult-hunks--comment-history nil
  "History of the notes left by `consult-hunks-comment'.")

(defun consult-hunks--note-line-p ()
  "Non-nil when the line at point is a note left above a change.
Nothing but comment syntax and whitespace comes before the tag of a
note, which is how the notes of earlier rounds are told apart from the
code they sit above."
  (string-match-p (concat "\\`[^[:alnum:]]*"
                          (regexp-quote consult-hunks-comment-tag))
                  (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))

(defun consult-hunks--write-comment (hunk text)
  "Write TEXT as a comment above the change of HUNK, and save the file.
It lands below the notes of earlier rounds, which sit right above the
change already, in the comment syntax of the file and indented like
the line it is written above.  The file is saved, the hunks of a
session being what git sees on disk and not what a buffer holds."
  (let ((name (alist-get 'file hunk))
        (file (expand-file-name (alist-get 'file hunk) consult-hunks--root)))
    (unless (file-exists-p file)
      (user-error "Nothing to leave a note in, %s is gone" name))
    ;; an open buffer is taken as it is, `find-file-noselect' would stop
    ;; to ask about one that is behind the file on disk
    (with-current-buffer (or (find-buffer-visiting file) (find-file-noselect file))
      (when (buffer-modified-p)
        (user-error "Save %s first, git has not seen what its buffer holds" name))
      ;; the hunks are of the file on disk, so a buffer left from before
      ;; it changed is brought up to date - the note would land at lines
      ;; that are no longer there otherwise, and be saved over the file
      (unless (verify-visited-file-modtime (current-buffer))
        (revert-buffer t t t))
      (unless comment-start
        (user-error "No comment syntax in %s" major-mode))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- (alist-get 'line hunk)))
          (while (and (not (eobp)) (consult-hunks--note-line-p))
            (forward-line 1))
          (let ((start (point))
                ;; `indent' is the style that puts the comment after
                ;; the indentation instead of in column zero
                (comment-style 'indent)
                (indentation (buffer-substring-no-properties
                              (line-beginning-position)
                              (save-excursion (back-to-indentation) (point)))))
            (insert indentation consult-hunks-comment-tag ": " text "\n")
            (comment-region start (1- (point))))))
      (save-buffer))))

(defun consult-hunks-comment ()
  "Leave a note in a comment above the selected hunk.
The note is written right above the change, in the comment syntax of
the file - a `FIXME: \=' for you, or for the AI you hand the file to,
to run into next to the change it is about.  A second note on the same
change lands below the first, so they read in the order they were
written.  The file is saved and the session restarts on the hunks as
they are then, the note being a change of its own by that point."
  (interactive)
  (when-let* ((hunk (consult-hunks--selection))
              (text (string-trim
                     (let ((enable-recursive-minibuffers t))
                       (read-string
                        (format "%s above %s:%s: " consult-hunks-comment-tag
                                (alist-get 'file hunk) (alist-get 'line hunk))
                        nil 'consult-hunks--comment-history))))
              ((not (string-empty-p text))))
    (consult-hunks--write-comment hunk text)
    (consult-hunks--refresh hunk)
    ;; back to where the note was left, the note being part of the hunk
    ;; of that change by now
    (consult-hunks--restart-session (consult-hunks--where hunk))))

(defun consult-hunks-cycle-diffs ()
  "Cycle what the preview of the selected hunk shows.
The hunk where it lives, then its diff inline after the line the hunk
starts at, then the file as git has it beside the file the hunk
changes it into, both scrolled to the hunk.  The preview is drawn
again through the session, the buffers it owns are not guaranteed to
be alive between previews."
  (interactive)
  (pcase (list consult-hunks-show-diffs consult-hunks-show-split)
    ('(nil nil) (setq consult-hunks-show-diffs t
                      consult-hunks-show-split nil))
    (`(,_ nil) (setq consult-hunks-show-diffs nil
                     consult-hunks-show-split t))
    (_ (setq consult-hunks-show-diffs nil
             consult-hunks-show-split nil)))
  (unless consult-hunks-show-split
    (consult-hunks--close-split))
  (when consult-hunks--preview
    (funcall consult-hunks--preview (consult-hunks--selection))))

(defun consult-hunks-find-other-window ()
  "Jump to the selected hunk in another window."
  (interactive)
  (when-let* ((hunk (consult-hunks--selection)))
    (consult-hunks--quit-and-run
     (lambda ()
       (let ((consult--buffer-display #'switch-to-buffer-other-window))
         (consult--jump (consult-hunks--position hunk)))))))

(defun consult-hunks-find-other-frame ()
  "Jump to the selected hunk in another frame."
  (interactive)
  (when-let* ((hunk (consult-hunks--selection)))
    (consult-hunks--quit-and-run
     (lambda ()
       (let ((consult--buffer-display #'switch-to-buffer-other-frame))
         (consult--jump (consult-hunks--position hunk)))))))

(defun consult-hunks-commit ()
  "Quit and commit with `consult-hunks-commit-fn'."
  (interactive)
  (when (commandp consult-hunks-commit-fn)
    (consult-hunks--quit-and-run
     (lambda () (call-interactively consult-hunks-commit-fn)))))

(defun consult-hunks-commit-amend ()
  "Quit and amend with `consult-hunks-commit-amend-fn'."
  (interactive)
  (when (commandp consult-hunks-commit-amend-fn)
    (consult-hunks--quit-and-run
     (lambda () (call-interactively consult-hunks-commit-amend-fn)))))

(defvar-keymap consult-hunks-map
  :doc "Keymap used while `consult-hunks' is open."
  "C-s" #'consult-hunks-stage
  "C-u" #'consult-hunks-unstage
  "C-k" #'consult-hunks-kill
  "C-c C-f" #'consult-hunks-comment
  "C-c C-p" #'consult-hunks-cycle-diffs
  "C-c o" #'consult-hunks-find-other-window
  "C-c C-o" #'consult-hunks-find-other-frame
  "C-c C-c" #'consult-hunks-commit
  "C-c C-a" #'consult-hunks-commit-amend
  "C-c C-k" #'abort-recursive-edit)

(defvar-keymap consult-hunks-normal-map
  :doc "Keymap used while `consult-hunks' is open and evil is in normal state.
The motion commands are remapped by the completion UI, `vertico-next'
and `vertico-previous' for vertico."
  "j" #'next-line
  "k" #'previous-line
  "s" #'consult-hunks-stage
  "u" #'consult-hunks-unstage
  "c" #'consult-hunks-comment)

(defvar consult-hunks--map-active nil
  "Non-nil in the minibuffer of a running session.")

(defvar consult-hunks--emulation-alist
  `((evil-normal-state-minor-mode . ,consult-hunks-normal-map)
    (consult-hunks--map-active . ,consult-hunks-map))
  "Emulation keymap alist activating the keymaps of a session.
The plain keys of `consult-hunks-normal-map' only apply while evil is
in normal state; the entry is ignored when evil is not installed, its
variable is unbound then.")

(defun consult-hunks--setup-keymap ()
  "Give `consult-hunks-map' precedence over every other keymap.
The `:keymap' consult installs ends up in the minibuffer local map,
which loses to the emulation maps evil keeps its state bindings in -
`evil-insert-state-map' would shadow \\`C-k\' with `kill-line\'.
Only the keys of `consult-hunks-map' are affected."
  (setq-local consult-hunks--map-active t)
  (setq-local emulation-mode-map-alists
              (cons 'consult-hunks--emulation-alist emulation-mode-map-alists))
  (when-let* ((state consult-hunks--entry-state))
    (setq consult-hunks--entry-state nil)
    (when (fboundp 'evil-change-state)
      (evil-change-state state))))

;;;; Entry points

(defun consult-hunks--current-file ()
  "Return the file of the current buffer, relative to the repository root."
  (let ((file (or (buffer-file-name) (user-error "Buffer is not visiting a file"))))
    (file-relative-name file (consult-hunks--find-root))))

(defun consult-hunks--run (staged file &optional root preselect)
  "Browse the hunks of the repository, staged ones when STAGED.
Only the hunks of FILE are shown when it is non-nil.  ROOT is the
repository root, found from `default-directory' when it is nil.
PRESELECT is the hunk to start on, a cons of a file and the line a
hunk of it starts at, as `consult-hunks--restart-session' hands it
over so a session picks up where the one before it left off."
  (let* ((root (or root (consult-hunks--find-root)))
         (consult-hunks--root root)
         (consult-hunks--staged staged)
         ;; saved before the minibuffer is entered, so the side by side
         ;; view has a configuration to put back that knows nothing of
         ;; a minibuffer
         (consult-hunks--wconf (current-window-configuration))
         (consult-hunks--split nil)
         (consult-hunks--split-built nil)
         (consult-hunks--blob-buffers nil)
         ;; the restart runs from a timer, where `default-directory' is
         ;; whatever buffer happens to be current, so carry the root
         (consult-hunks--restart
          (lambda (&optional preselect)
            (consult-hunks--run staged file root preselect)))
         (hunks (consult-hunks--hunks file))
         (consult-hunks--session-hunks hunks)
         (candidates (mapcar #'consult-hunks--candidate hunks)))
    (unless hunks
      (user-error (if staged "No staged changes" "No changes")))
    (consult-hunks--kill-blob-buffers)
    (unwind-protect
        (minibuffer-with-setup-hook
            (:append (lambda ()
                       (consult-hunks--setup-keymap)
                       (consult-hunks--select-hunk preselect hunks candidates)))
          (consult--read
           candidates
           :prompt (format "%s hunks%s: "
                           (if staged "Staged" "Changed")
                           (if file (concat " in " file) ""))
           :category 'consult-hunks
           :require-match t
           :sort nil
           :lookup #'consult--lookup-candidate
           :group #'consult--prefix-group
           :state (consult-hunks--state)
           :keymap consult-hunks-map
           :history 'consult-hunks--history))
      (consult-hunks--restore-wconf)
      (consult-hunks--kill-blob-buffers))
    nil))

;;;###autoload
(defun consult-hunks ()
  "Browse the unstaged hunks of the project."
  (interactive)
  (consult-hunks--run nil nil))

;;;###autoload
(defun consult-hunks-current-buffer ()
  "Browse the unstaged hunks of the current buffer."
  (interactive)
  (consult-hunks--run nil (consult-hunks--current-file)))

;;;###autoload
(defun consult-hunks-staged ()
  "Browse the staged hunks of the project."
  (interactive)
  (consult-hunks--run t nil))

;;;###autoload
(defun consult-hunks-staged-current-buffer ()
  "Browse the staged hunks of the current buffer."
  (interactive)
  (consult-hunks--run t (consult-hunks--current-file)))

(provide 'consult-hunks)

;;; consult-hunks.el ends here
