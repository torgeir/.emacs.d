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
;; by vertico, so the added and deleted text can be narrowed on.  Toggle
;; the same diff inline where the hunk lives with `C-c C-p'.  Jump to a
;; hunk in "other window" or "other frame" with `C-c o' and `C-c C-o'.
;;
;; Commit with `C-c C-c', amend with `C-c C-a', quit with `C-c C-k'.
;;
;; Evil users get plain keys in normal state as well: `j' and `k' to
;; move, `s' to stage and `u' to unstage.  The rest of normal state is
;; left alone, `p' still pastes.
;;
;; Staging, unstaging and killing act on the selected hunk and then
;; restart the session on the refreshed list of hunks, so hunks can be
;; staged one after the other.
;;
;; Credits/inspiration: helm-hunks - https://github.com/torgeir/helm-hunks.el

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'subr-x)

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
Toggled by `consult-hunks-toggle-diffs' while the session is open.
The candidates always carry their diff, but vertico collapses it onto
the candidate line, so it reads as one line and is there to narrow on."
  :type 'boolean)

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
starts at, the `type' of change, the `content' of the diff and the
`patch' that applies it on its own."
  (let ((hunk-header (car lines))
        ;; with --unified=0 every content line is an addition, a deletion
        ;; or the no-newline marker, which drops the trailing empty line
        (content (seq-filter (lambda (line) (string-match-p "\\`[-+\\]" line))
                             (cdr lines))))
    (when (string-match consult-hunks--hunk-re hunk-header)
      (let* ((del-len (string-to-number (or (match-string 2 hunk-header) "1")))
             (add-line (string-to-number (match-string 3 hunk-header)))
             (add-len (string-to-number (or (match-string 4 hunk-header) "1")))
             (type (cond ((zerop del-len) 'added)
                         ((zerop add-len) 'deleted)
                         (t 'modified))))
        `((file . ,file)
          ;; a deleted hunk sits between add-line and the line after it
          (line . ,(if (zerop add-len) (1+ add-line) add-line))
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

(defun consult-hunks--state ()
  "State function previewing the hunk where it lives."
  (let ((open (consult--temporary-files))
        (jump (consult--jump-state)))
    (setq consult-hunks--open open)
    (lambda (action hunk)
      (unless hunk (funcall open))
      (when (eq action 'return) (setq consult-hunks--open nil))
      (consult-hunks--hide-diff)
      (let ((pos (consult-hunks--position
                  hunk (and (not (eq action 'return)) open))))
        (funcall jump action pos)
        (when (and (eq action 'preview) pos consult-hunks-show-diffs)
          (consult-hunks--show-diff pos hunk))))))

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

(defun consult-hunks--restart-session ()
  "Quit and restart the session on the refreshed hunks.
The evil state is carried over, as staging from normal state would
otherwise land in the insert state every new minibuffer starts in."
  (setq consult-hunks--entry-state (bound-and-true-p evil-state))
  (consult-hunks--quit-and-run consult-hunks--restart))

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
REVERT is passed to `consult-hunks--refresh'."
  (when-let* ((hunk (consult-hunks--selection)))
    (funcall fn hunk)
    (consult-hunks--refresh hunk revert)
    (consult-hunks--restart-session)))

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

(defun consult-hunks-toggle-diffs ()
  "Toggle the diff shown inline at the previewed hunk."
  (interactive)
  (setq consult-hunks-show-diffs (not consult-hunks-show-diffs))
  (consult-hunks--hide-diff)
  (when consult-hunks-show-diffs
    ;; draw through the opener of the session, the preview buffers it
    ;; owns are not guaranteed to be alive between previews
    (when-let* ((hunk (consult-hunks--selection))
                (pos (consult-hunks--position hunk consult-hunks--open)))
      (consult-hunks--show-diff pos hunk))))

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
  "C-c C-p" #'consult-hunks-toggle-diffs
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
  "u" #'consult-hunks-unstage)

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

(defun consult-hunks--run (staged file &optional root)
  "Browse the hunks of the repository, staged ones when STAGED.
Only the hunks of FILE are shown when it is non-nil.  ROOT is the
repository root, found from `default-directory' when it is nil."
  (let* ((root (or root (consult-hunks--find-root)))
         (consult-hunks--root root)
         (consult-hunks--staged staged)
         ;; the restart runs from a timer, where `default-directory' is
         ;; whatever buffer happens to be current, so carry the root
         (consult-hunks--restart (lambda () (consult-hunks--run staged file root)))
         (hunks (consult-hunks--hunks file)))
    (unless hunks
      (user-error (if staged "No staged changes" "No changes")))
    (minibuffer-with-setup-hook (:append #'consult-hunks--setup-keymap)
      (consult--read
       (mapcar #'consult-hunks--candidate hunks)
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
