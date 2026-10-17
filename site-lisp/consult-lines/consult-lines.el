;;; consult-lines.el --- Complete the current line by lines in the project -*- lexical-binding: t; -*-

;; Author: @torgeir
;; Version: 1.0.0
;; Keywords: files consult rg vc git lines complete tools languages
;; Package-Requires: ((emacs "28.1") (consult "3.0"))

;;; Commentary:

;; A consult interface for completing by lines in a project, a
;; re-implementation of `helm-lines' on top of consult and ripgrep.
;;
;; Run `consult-lines' to complete the current line by other lines that
;; exist in the current project.  The current line is used as the
;; initial query, and the candidate under the cursor replaces the line
;; in place as you move around the candidate list, the way
;; `helm-follow-mode' did.  Abort with `C-g' to restore the line.
;;
;; The project root is the result of
;; `consult-lines-project-root-function', by default the root of the
;; current version-control tree.
;;
;; The search is done by `rg' [1] with the arguments in
;; `consult-lines-args'.  The input is split at spaces, and every part
;; has to match the line, in any order, so `console b' finds
;; `console.log(b)'.  The parts are matched literally, as the current
;; line is full of characters that are special in a regexp.  Set
;; `consult-lines-regexp-compiler' to `consult--default-regexp-compiler'
;; to type regexps instead, or escape a space as `\\ ' to search for a
;; literal space.
;;
;;   [1]: https://github.com/BurntSushi/ripgrep

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'subr-x)
(require 'thingatpt)

(defgroup consult-lines nil
  "Completion by lines in project."
  :group 'consult)

(defcustom consult-lines-args
  "rg --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --no-filename --no-line-number --trim\
   --search-zip --glob !.git --glob !target --glob !node_modules"
  "Command line arguments for ripgrep, see `consult-lines'.
Bound to `consult-ripgrep-args' while the search runs, as the command
line is built by `consult--ripgrep-make-builder'.  The dynamically
computed arguments are appended.
Can be either a string, or a list of strings or expressions."
  :type '(choice string (repeat (choice string sexp))))

(defcustom consult-lines-project-root-function #'consult-lines--project-root
  "Function called to find the root directory of the current project."
  :type 'function)

(defcustom consult-lines-regexp-compiler #'consult-lines--literal-regexp-compiler
  "Regexp compiler used to turn the input into ripgrep patterns.
Defaults to matching every space separated part of the input
literally.  Set it to `consult--default-regexp-compiler' to treat the
input as an Emacs regexp instead, as `consult-ripgrep' does."
  :type 'function)

(defcustom consult-lines-split-style 'none
  "Async splitting style used by `consult-lines'.
Defaults to `none', so the whole input, current line included, is
passed to ripgrep.  See `consult-async-split-styles-alist'."
  :type 'symbol)

(defvar consult-lines-history nil
  "Input history of `consult-lines'.")

(defun consult-lines--project-root ()
  "Return the root of the current project, or nil."
  (or (when-let* ((project (project-current nil)))
        (project-root project))
      (vc-root-dir)))

(defun consult-lines--literal-regexp-compiler (input type ignore-case)
  "Compile INPUT to a list of literal regexps of TYPE.
Like `consult--default-regexp-compiler', but every space separated
part of the INPUT is matched literally instead of as a regexp, as the
line being completed is full of regexp specials.  IGNORE-CASE is
passed on to the highlighting function."
  (let ((parts (consult--split-escaped input)))
    (cons (mapcar (lambda (part)
                    (consult--convert-regexp (regexp-quote part) type))
                  parts)
          (apply-partially #'consult--highlight-literals parts ignore-case))))

(defun consult-lines--transform ()
  "Async function dropping empty and deduplicating lines.
Leading whitespace is already gone, ripgrep is called with `--trim'.
The seen table is rebuilt per input, since the candidate list is
flushed whenever the input changes."
  (consult--async-transform-by-input
   (lambda (_input)
     (let ((seen (make-hash-table :test #'equal)))
       (lambda (cands)
         (let (result)
           (dolist (cand cands (nreverse result))
             (let ((line (string-trim-right cand)))
               (unless (or (string-empty-p line) (gethash line seen))
                 (puthash line t seen)
                 (push line result))))))))))

(defun consult-lines--replace (beg end line &optional indent)
  "Replace the text between BEG and END by LINE, indenting it when INDENT.
Leaves point at the end of the inserted LINE."
  (goto-char beg)
  (delete-region beg end)
  (insert (substring-no-properties line))
  (when indent
    (indent-according-to-mode)))

(defun consult-lines--state ()
  "State function replacing the current line by the candidate.
The replacement is undone when the preview moves on, and it is kept
out of the undo history, so only the selected line ends up there."
  (let* ((buffer (current-buffer))
         (beg (copy-marker (line-beginning-position)))
         (end (copy-marker (line-end-position) t))
         (original (buffer-substring-no-properties beg end))
         (previewing nil))
    (cl-flet ((preview-line (line indent)
                ;; keep the previews out of the undo history, only the
                ;; selected line should end up there
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (let ((buffer-undo-list t))
                      (save-excursion
                        (consult-lines--replace beg end line indent))))))
              (insert-line (line)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (consult-lines--replace beg end line t)))))
      (lambda (action cand)
        (pcase action
          ('preview
           (cond
            ((and cand (not (string-empty-p cand)))
             (preview-line cand t)
             (setq previewing t))
            (previewing
             ;; restore the line as it was, indentation included
             (preview-line original nil)
             (setq previewing nil))))
          ('return
           (when previewing
             (preview-line original nil)
             (setq previewing nil))
           (when (and cand (not (string-empty-p cand)))
             (insert-line cand))
           (set-marker beg nil)
           (set-marker end nil)))))))

;;;###autoload
(defun consult-lines (&optional initial)
  "Complete the current line by lines found in the project.
INITIAL is the initial input, by default the current line."
  (interactive)
  (unless (executable-find "rg")
    (user-error "Consult-lines: Could not find `rg', see https://github.com/BurntSushi/ripgrep"))
  (let* ((root (file-name-as-directory
                (expand-file-name (or (funcall consult-lines-project-root-function)
                                      (error "Couldn't determine project root")))))
         (consult-ripgrep-args consult-lines-args)
         (consult--regexp-compiler consult-lines-regexp-compiler)
         (consult-async-split-style consult-lines-split-style)
         (builder (consult--ripgrep-make-builder (list root))))
    (consult--read
     (consult--process-collection builder
       :transform (consult-lines--transform)
       :highlight t
       :file-handler t)
     :prompt (format "Complete line in %s: "
                     (file-name-nondirectory (directory-file-name root)))
     :initial (or initial (string-trim (or (thing-at-point 'line t) "")))
     :state (consult-lines--state)
     :add-history (thing-at-point 'symbol)
     :require-match nil
     :sort nil
     :category 'consult-lines
     :history '(:input consult-lines-history))
    nil))

(provide 'consult-lines)

;;; consult-lines.el ends here
