;;; vertico-hunks.el --- A vertico interface for git hunks - browsing, staging, unstaging and killing -*- lexical-binding: t -*-

;; Copyright (C) 2012-2016 Free Software Foundation, Inc.

;; Author: @torgeir
;; Version: 2.0.0
;; Keywords: vertico git hunks vc
;; Package-Requires: ((emacs "28.1") (vertico "1.0") (marginalia "1.0") (embark "1.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A vertico interface for git hunks - browsing, staging, unstaging and killing.
;;
;; Run `vertico-hunks' to jump around unstaged hunks. Navigate with up/down
;; arrows to preview hunks live. Stage with `C-c s', unstage with `C-c u', 
;; and kill hunks with `C-c k'.
;;
;; Run `vertico-hunks-current-buffer' to jump around the current buffer only.
;;
;; Run `vertico-hunks-staged' to jump around staged hunks.
;;
;; Run `vertico-hunks-staged-current-buffer' to jump around staged hunks in
;; the current buffer only.
;;
;; Use embark actions with `C-.' for additional operations.
;;
;; Toggle preview with `C-c C-p'.
;;
;; Credits/inspiration: git-gutter+ - https://github.com/nonsequitur/git-gutter-plus/

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup vertico-hunks nil
  "A vertico interface for git hunks"
  :group 'vc)

(defcustom vertico-hunks-refresh-hook
  nil
  "Hooks triggered whenever `vertico-hunks' trigger git changes, so you can refresh your favorite git-gutter on git changes."
  :type 'hook
  :group 'vertico-hunks)

(defcustom vertico-hunks-enable-preview
  t
  "Enable live preview when navigating hunks."
  :type 'boolean
  :group 'vertico-hunks)

(defvar vertico-hunks-commit-fn
  'magit-commit
  "Defun to call interactively for committing, defaults to magit.")

(defvar vertico-hunks-commit-amend-fn
  'magit-commit-amend
  "Defun to call interactively for amending, defaults to magit.")

(defvar vertico-hunks-preview-diffs
  t
  "Enable preview. Shows diff lines preview in annotations.")

(defconst vertico-hunks--diff-re
  "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
  "Regex to match the git diff hunk lines, e.g `@@ -{del-line},{del-len} +{add-line},{add-len} @@'.")

(defvar vertico-hunks--cmd-file-names
  "git --no-pager diff --name-only"
  "Git command to return names of the changed files.")

(defvar vertico-hunks--cmd-diffs
  "git --no-pager diff --no-color --no-ext-diff --unified=0"
  "Git command to show minimal diffs.")

(defconst vertico-hunks--cmd-git-root
  "git rev-parse --show-toplevel"
  "Git command to find the root folder of the current repo.")

(defconst vertico-hunks--cmd-git-apply
  "git apply --unidiff-zero --cached -"
  "Git command to apply the patch read from stdin.")

(defconst vertico-hunks--cmd-git-apply-reverse
  "git apply --unidiff-zero --cached --reverse -"
  "Git command to apply the patch read from stdin in reverse.")

(defconst vertico-hunks--msg-no-changes
  "No changes."
  "Message shown when there are no changed hunks.")

(defconst vertico-hunks--msg-no-changes-staged
  "No staged changes."
  "Message shown when there are no staged hunks.")

(defvar vertico-hunks--is-staged
  nil
  "Is showing staged hunks.")

(defvar vertico-hunks--current-hunks
  nil
  "Current list of hunks for the completion session.")

(defvar vertico-hunks--preview-buffer
  nil
  "Buffer used for previewing hunks.")

;; Refresh git-gutter+ on git changes
(when (and (boundp 'git-gutter+-mode)
           git-gutter+-mode
           (fboundp 'git-gutter+-refresh))
  (add-hook 'vertico-hunks-refresh-hook 'git-gutter+-refresh))

(defun vertico-hunks--take (n lst)
  "Take `N' elements of `LST'."
  (butlast lst (- (length lst) n)))

(defun vertico-hunks--msg-no-hunks ()
  "Message to show when there are no hunks to display."
  (if vertico-hunks--is-staged
      vertico-hunks--msg-no-changes-staged
    vertico-hunks--msg-no-changes))

(defun vertico-hunks--get-file-names ()
  "List file names of changed files."
  (let* ((result (shell-command-to-string vertico-hunks--cmd-file-names))
         (raw-file-names (split-string result "\r?\n")))
    (delete "" raw-file-names)))

(defun vertico-hunks--get-diffs ()
  "List raw diffs per changed file."
  (let* ((result (shell-command-to-string vertico-hunks--cmd-diffs))
         (split-diff-lines (split-string result "^diff --git a/"))
         (split-diff-lines-without-empties (delete "" split-diff-lines)))
    (mapcar (lambda (line)
              (concat "diff --git a/" line))
            split-diff-lines-without-empties)))

(defun vertico-hunks--extract-hunk-lines (diff)
  "Split `DIFF' string on ^@@ to group lists of each hunk's header and content lines in a list."
  (mapcar (lambda (hunk)
            (concat "@@" hunk))
          (delete "" (split-string diff "^@@"))))

(defun vertico-hunks--get-git-root ()
  "Get the root folder of the current git repository."
  (let ((result (shell-command-to-string vertico-hunks--cmd-git-root)))
    (file-name-as-directory
     (replace-regexp-in-string "\r?\n" "" result))))

(defun vertico-hunks--parse-hunk (diff-header-str hunk-str)
  "Join `DIFF-HEADER-STR' and the parsed `HUNK-STR' into a hunk."
  (let* ((hunk-lines (split-string hunk-str "\n"))
         (hunk-header-line (car hunk-lines))
         (content-lines (cdr hunk-lines)))
    (when (string-match vertico-hunks--diff-re hunk-header-line)
      (let* ((del-len (string-to-number (or (match-string 2 hunk-header-line) "1")))
             (add-line (string-to-number (match-string 3 hunk-header-line)))
             (add-len (string-to-number (or (match-string 4 hunk-header-line) "1")))
             (content (string-join content-lines "\n"))
             (type (cond ((zerop del-len) 'added)
                         ((zerop add-len) 'deleted)
                         (t 'modified)))
             (is-deleted (eq type 'deleted))
             (line (if is-deleted (1+ add-line) add-line))
             (line-end (if is-deleted line (1- (+ add-line add-len)))))
        (list (cons 'diff-header diff-header-str)
              (cons 'hunk-header hunk-header-line)
              (cons 'content content)
              (cons 'raw-content (concat diff-header-str "\n" hunk-str))
              (cons 'type type)
              (cons 'line line)
              (cons 'line-end line-end))))))

(defun vertico-hunks--assoc-file-name (file-name hunks)
  "Associates `FILE-NAME' name with each hunk of the `HUNKS' list."
  (mapcar (lambda (hunk)
            (cons (cons 'file file-name)
                  hunk))
          hunks))

(defun vertico-hunks--take-before-diff (acc l)
  "Grab all lines before the one starting with @@."
  (let ((head (car l)))
    (if (or (null head)
            (string-match-p "^@@.*" head))
        acc
      (vertico-hunks--take-before-diff (append acc (list head)) (cdr l)))))

(defun vertico-hunks--drop-before-diff (l)
  "Throw away all lines before the one starting with @@."
  (let ((head (car l)))
    (if (or (not head)
            (string-match-p "^@@.*" head))
        l
      (vertico-hunks--drop-before-diff (cdr l)))))

(defun vertico-hunks--get-hunks-by-file (file-names diffs-per-file)
  "Join the changed file names with their corresponding hunks in a list."
  (cl-loop for file-name in file-names
           for diff-str in diffs-per-file
           collect (let* ((split-hunk (split-string diff-str "\r?\n"))
                          (diff-header-lines (vertico-hunks--take-before-diff '() split-hunk))
                          (diff-header-str (string-join diff-header-lines "\n"))
                          (rest-str (string-join (vertico-hunks--drop-before-diff split-hunk) "\n"))
                          (hunks-lines (vertico-hunks--extract-hunk-lines rest-str))
                          (parsed-hunks (mapcar (lambda (hunk-lines)
                                                  (vertico-hunks--parse-hunk diff-header-str hunk-lines))
                                                hunks-lines))
                          (parsed-hunks-with-file (vertico-hunks--assoc-file-name file-name parsed-hunks)))
                     (cons file-name parsed-hunks-with-file))))

(defun vertico-hunks--fontify-as-diff (content)
  "Fontify `CONTENT' as a diff, like it's shown in `diff-mode'."
  (with-temp-buffer
    (insert content)
    (diff-mode)
    (font-lock-default-function 'diff-mode)
    (font-lock-default-fontify-buffer)
    (buffer-string)))

(defun vertico-hunks--format-candidate (hunk)
  "Format `HUNK' for display as a completion candidate."
  (let ((file (cdr (assoc 'file hunk))))
    (unless (equal file (vertico-hunks--msg-no-hunks))
      (let* ((line (cdr (assoc 'line hunk)))
             (type (cdr (assoc 'type hunk))))
        (format "%s:%s (%s)" file line type)))))

(defun vertico-hunks--get-hunks ()
  "Create a list of hunks for completion."
  (reverse
   (let* ((hunks-by-file (vertico-hunks--get-hunks-by-file (vertico-hunks--get-file-names)
                                                           (vertico-hunks--get-diffs)))
          (changes nil))
     (dolist (hunk-by-file hunks-by-file changes)
       (let ((hunks (cdr hunk-by-file)))
         (dolist (hunk hunks)
           (let ((formatted (vertico-hunks--format-candidate hunk)))
             (when formatted
               (push (cons formatted hunk) changes)))))))))

(defun vertico-hunks--find-hunk (candidate)
  "Find the hunk data for the given CANDIDATE string."
  (cdr (assoc candidate vertico-hunks--current-hunks)))

(defun vertico-hunks--find-hunk-with-fn (hunk find-file-fn)
  "Jump to the changed line in the file of the `HUNK' using the provided `FIND-FILE-FN' function."
  (let* ((file (cdr (assoc 'file hunk)))
         (line (cdr (assoc 'line hunk)))
         (file-path (concat (vertico-hunks--get-git-root) file)))
    (funcall find-file-fn file-path)
    (goto-char (point-min))
    (forward-line (1- line))))

(defvar-keymap vertico-hunks-minibuffer-map
  :doc "Keymap for vertico-hunks minibuffer"
  "C-s" (lambda () (interactive) 
          (when-let ((candidate (vertico--candidate)))
            (vertico-hunks--stage-hunk candidate)
            (vertico--exhibit)))
  "C-u" (lambda () (interactive)
          (when-let ((candidate (vertico--candidate)))
            (vertico-hunks--unstage-hunk candidate)
            (vertico--exhibit)))
  "C-k" (lambda () (interactive)
          (when-let ((candidate (vertico--candidate)))
            (vertico-hunks--revert-hunk candidate)
            (exit-minibuffer)))
  "C-c C-p" #'vertico-hunks--toggle-preview
  "C-c C-c" (lambda () (interactive)
              (exit-minibuffer)
              (when (fboundp vertico-hunks-commit-fn)
                (funcall vertico-hunks-commit-fn)))
  "C-c C-a" (lambda () (interactive)
              (exit-minibuffer)
              (when (fboundp vertico-hunks-commit-amend-fn)
                (funcall vertico-hunks-commit-amend-fn))))

(defun vertico-hunks--run-cmd-on-hunk (cmd hunk)
  "Run git `CMD' on `HUNK'."
  (let ((raw-hunk-diff (cdr (assoc 'raw-content hunk))))
    (with-temp-buffer
      (insert raw-hunk-diff)
      (unless (zerop
               (shell-command-on-region
                (point-min)
                (point-max)
                (format "cd %s && %s"
                        (shell-quote-argument (vertico-hunks--get-git-root))
                        cmd)
                t t nil))
        (buffer-string)))))

(defun vertico-hunks--stage-hunk (candidate)
  "Stage the hunk represented by CANDIDATE."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (vertico-hunks--run-cmd-on-hunk vertico-hunks--cmd-git-apply hunk)
      (run-hooks 'vertico-hunks-refresh-hook)
      ;; Refresh the candidate list
      (setq vertico-hunks--current-hunks (vertico-hunks--get-hunks))
      (setq vertico--candidates (mapcar #'car vertico-hunks--current-hunks))
      (vertico--exhibit)
      (message "Staged hunk"))))

(defun vertico-hunks--unstage-hunk (candidate)
  "Unstage the hunk represented by CANDIDATE."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (vertico-hunks--run-cmd-on-hunk vertico-hunks--cmd-git-apply-reverse hunk)
      (run-hooks 'vertico-hunks-refresh-hook)
      ;; Refresh the candidate list
      (setq vertico-hunks--current-hunks (vertico-hunks--get-hunks))
      (setq vertico--candidates (mapcar #'car vertico-hunks--current-hunks))
      (vertico--exhibit)
      (message "Unstaged hunk"))))

(defun vertico-hunks--revert-collect-deleted-lines (content)
  "Collects the deleted lines from the `CONTENT's of a hunk."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^-\\(.*?\\)$" nil t)
             collect (match-string 1) into deleted-lines
             finally return deleted-lines)))

(defun vertico-hunks--revert-added-hunk (hunk)
  "Reverts lines added by `HUNK' by deleting them."
  (let ((line (cdr (assoc 'line hunk)))
        (line-end (cdr (assoc 'line-end hunk)))
        (start-point (point)))
    (forward-line (1+ (- line-end line)))
    (delete-region start-point (point))))

(defun vertico-hunks--revert-deleted-hunk (hunk)
  "Reverts lines deleted by `HUNK' by adding them back."
  (let ((content (cdr (assoc 'content hunk))))
    (dolist (line (vertico-hunks--revert-collect-deleted-lines content))
      (insert (concat line "\n")))))

;; Marginalia integration
(defun vertico-hunks--marginalia-annotate (candidate)
  "Provide marginalia annotation for hunk CANDIDATE."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (let* ((type (cdr (assoc 'type hunk)))
             (content (cdr (assoc 'content hunk)))
             (preview (if (and vertico-hunks-preview-diffs (not (string-empty-p content)))
                          (concat " " (propertize 
                                       (replace-regexp-in-string "\n" " " 
                                                                 (truncate-string-to-width content 50))
                                       'face 'marginalia-documentation))
                        "")))
        (concat (propertize (format " [%s]" type) 'face 'marginalia-type)
                preview)))))

(add-to-list 'marginalia-annotator-registry
             '(vertico-hunks vertico-hunks--marginalia-annotate builtin none))

;; Embark integration
(require 'embark)
(defvar-keymap vertico-hunks-embark-map
  :doc "Embark keymap for git hunks"
  :parent embark-general-map
  "s" #'vertico-hunks--stage-hunk
  "u" #'vertico-hunks--unstage-hunk  
  "k" #'vertico-hunks--revert-hunk
  "o" #'vertico-hunks--action-find-hunk-other-window
  "C-o" #'vertico-hunks--action-find-hunk-other-frame)

(add-to-list 'embark-keymap-alist '(vertico-hunks . vertico-hunks-embark-map))

(defun vertico-hunks--embark-classify (_cands)
  "Classify completion candidates as git hunks."
  'vertico-hunks)

(defun vertico-hunks--action-find-hunk (candidate)
  "Jump to the file of the hunk represented by CANDIDATE."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (vertico-hunks--find-hunk-with-fn hunk #'find-file))))

(defun vertico-hunks--action-find-hunk-other-window (candidate)
  "Jump to the changed line in the file of the hunk in other window."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (vertico-hunks--find-hunk-with-fn hunk #'find-file-other-window))))

(defun vertico-hunks--action-find-hunk-other-frame (candidate)
  "Jump to the changed line in the file of the hunk in other frame."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (vertico-hunks--find-hunk-with-fn hunk #'find-file-other-frame))))

(defun vertico-hunks--revert-hunk (candidate)
  "Revert the hunk represented by CANDIDATE."
  (let ((hunk (vertico-hunks--find-hunk candidate)))
    (when hunk
      (vertico-hunks--find-hunk-with-fn hunk #'find-file)
      (let* ((type (cdr (assoc 'type hunk)))
             (revert-fn (cl-case type
                          (added #'vertico-hunks--revert-added-hunk)
                          (deleted #'vertico-hunks--revert-deleted-hunk)
                          (modified (lambda (hunk)
                                      (vertico-hunks--revert-added-hunk hunk)
                                      (vertico-hunks--revert-deleted-hunk hunk))))))
        (funcall revert-fn hunk)
        (save-buffer)
        (run-hooks 'vertico-hunks-refresh-hook)
        (message "Reverted hunk")))))

(defun vertico-hunks--preview-hunk (hunk)
  "Show preview of HUNK in the preview buffer."
  (when (and vertico-hunks-enable-preview hunk)
    (let* ((file (cdr (assoc 'file hunk)))
           (line (cdr (assoc 'line hunk)))
           (type (cdr (assoc 'type hunk)))
           (content (cdr (assoc 'content hunk)))
           (file-path (concat (vertico-hunks--get-git-root) file))
           (buffer (find-file-noselect file-path)))
      (display-buffer buffer 'display-buffer-same-window)
      (with-current-buffer buffer
        (goto-line line)))
    ))

(defun vertico-hunks--preview-current-candidate ()
  "Preview the currently selected candidate."
  (when (and vertico-hunks-enable-preview
             (> (length vertico--candidates) 0))
    (let* ((candidate (vertico--candidate)))
      (when (and candidate vertico-hunks--current-hunks)
        (let ((hunk (vertico-hunks--find-hunk candidate)))
          (when hunk
            (vertico-hunks--preview-hunk hunk)))))))

(defun vertico-hunks--toggle-preview ()
  "Toggle preview mode."
  (interactive)
  (setq vertico-hunks-enable-preview (not vertico-hunks-enable-preview))
  (if vertico-hunks-enable-preview
      (progn
        (message "Preview enabled")
        (vertico-hunks--preview-current-candidate))
    (progn
      (message "Preview disabled"))))

;;;###autoload
(defun vertico-hunks ()
  "Browse git hunks with vertico."
  (interactive)
  (let* ((hunks (vertico-hunks--get-hunks))
         (vertico-hunks--current-hunks hunks)
         (candidates (mapcar #'car hunks)))
    (if (null candidates)
        (message (vertico-hunks--msg-no-hunks))
      (let* ((embark-classify-completion-function #'vertico-hunks--embark-classify)
             (completion-category-overrides 
              '((vertico-hunks (styles basic partial-completion)))))
        (unwind-protect
            (minibuffer-with-setup-hook
                (lambda ()
                  (use-local-map (make-composed-keymap vertico-hunks-minibuffer-map vertico-map))
                  (when vertico-hunks-enable-preview
                    (add-hook 'post-command-hook #'vertico-hunks--preview-current-candidate nil t))
                  )
              (let ((choice (completing-read "Git hunks: " candidates nil t)))
                (when choice
                  (vertico-hunks--action-find-hunk choice)))))))))

;;;###autoload
(defun vertico-hunks-current-buffer ()
  "Browse git hunks for the current buffer with vertico."
  (interactive)
  (let* ((current-file-relative (file-relative-name (buffer-file-name (current-buffer))))
         (vertico-hunks--cmd-diffs-single-file (format "%s -- %s" vertico-hunks--cmd-diffs current-file-relative))
         (vertico-hunks--cmd-file-names-single-file (format "%s -- %s" vertico-hunks--cmd-file-names current-file-relative))
         (vertico-hunks--cmd-diffs vertico-hunks--cmd-diffs-single-file)
         (vertico-hunks--cmd-file-names vertico-hunks--cmd-file-names-single-file))
    (vertico-hunks)))

;;;###autoload
(defun vertico-hunks-staged ()
  "Browse staged git hunks with vertico."
  (interactive)
  (let* ((vertico-hunks--is-staged t)
         (vertico-hunks--cmd-diffs-staged (format "%s --staged" vertico-hunks--cmd-diffs))
         (vertico-hunks--cmd-file-names-staged (format "%s --staged" vertico-hunks--cmd-file-names))
         (vertico-hunks--cmd-diffs vertico-hunks--cmd-diffs-staged)
         (vertico-hunks--cmd-file-names vertico-hunks--cmd-file-names-staged))
    (vertico-hunks)))

;;;###autoload
(defun vertico-hunks-staged-current-buffer ()
  "Browse staged git hunks for the current buffer with vertico."
  (interactive)
  (let* ((current-file-relative (file-relative-name (buffer-file-name (current-buffer))))
         (vertico-hunks--is-staged t)
         (vertico-hunks--cmd-diffs-staged (format "%s --staged" vertico-hunks--cmd-diffs))
         (vertico-hunks--cmd-file-names-staged (format "%s --staged" vertico-hunks--cmd-file-names))
         (vertico-hunks--cmd-diffs-single-file (format "%s -- %s" vertico-hunks--cmd-diffs-staged current-file-relative))
         (vertico-hunks--cmd-file-names-single-file (format "%s -- %s" vertico-hunks--cmd-file-names-staged current-file-relative))
         (vertico-hunks--cmd-diffs vertico-hunks--cmd-diffs-single-file)
         (vertico-hunks--cmd-file-names vertico-hunks--cmd-file-names-single-file))
    (vertico-hunks)))

(provide 'vertico-hunks)
;;; vertico-hunks.el ends here
