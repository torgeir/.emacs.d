;;; json-path-eldoc.el --- Show the path to the JSON value at point in eldoc -*- lexical-binding: t; -*-

;; Author: @torgeir
;; Version: 2.0.0
;; Keywords: json eldoc tree-sitter tools languages
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Show the path to the JSON value point is in, as `items[2].name', in
;; the eldoc echo area.
;;
;; Turn it on with `json-path-eldoc-mode' in a buffer, or add it to
;; `json-ts-mode-hook' to have it on in every JSON buffer.  Run
;; `json-path-eldoc-path' to put the path in the echo area and the kill
;; ring.
;;
;; The path is read off a tree-sitter tree, so it is O(depth) no matter
;; how big the file is - the old implementation copied the whole buffer
;; into a temp buffer and re-parsed it with `js2-mode' on every single
;; eldoc update, which is what made it crawl.  The tree-sitter `json'
;; grammar is used, and `json-ts-mode' already keeps a parser around, so
;; in that mode this is only a walk up from the node at point.
;;
;; Keys are separated by dots and array elements are indexed from 0, the
;; way jq writes them, so the path can be pasted into a jq filter.  A key
;; that is not a plain identifier is quoted in brackets instead, as
;; `items[2]["content-type"]'.  Set `json-path-eldoc-leading-dot' to t to
;; have the path start with a `.', which is what jq wants.

;;; Code:

(require 'treesit)
(require 'subr-x)

(defgroup json-path-eldoc nil
  "Show the path to the JSON value at point in eldoc."
  :group 'tools
  :prefix "json-path-eldoc-")

(defcustom json-path-eldoc-leading-dot nil
  "Whether the path starts with a dot, as `.items[2].name'.
That is the form jq takes as a filter."
  :type 'boolean
  :group 'json-path-eldoc)

(defconst json-path-eldoc--plain-key-re "\\`[a-zA-Z_][a-zA-Z0-9_]*\\'"
  "Keys matching this are written as `.key', the rest are quoted.")

(defun json-path-eldoc--parser ()
  "Return a tree-sitter parser for JSON in the current buffer, or nil.
The parser `json-ts-mode' already keeps is reused when there is one."
  (or (car (treesit-parser-list nil 'json))
      (and (treesit-ready-p 'json t)
           (treesit-parser-create 'json))))

(defun json-path-eldoc--key-name (key)
  "Return the text of the `string' node KEY without its quotes."
  (let ((content (treesit-node-child key 0 t)))
    (if (and content (equal (treesit-node-type content) "string_content"))
        (treesit-node-text content t)
      (string-trim (treesit-node-text key t) "\"" "\""))))

(defun json-path-eldoc--key-step (key)
  "Return the path step for the `string' node KEY, or nil.
A plain identifier is written as `.key', anything else is quoted as
`[\"a key\"]' so the path stays unambiguous."
  (when key
    (let ((name (json-path-eldoc--key-name key)))
      (if (string-match-p json-path-eldoc--plain-key-re name)
          (concat "." name)
        (format "[%S]" name)))))

(defun json-path-eldoc--child-position (parent child)
  "Return the position of CHILD among PARENT's named children.
The children are sorted by buffer position, so the position is found by
bisecting them - walking the siblings instead, the way
`treesit-node-index' does, takes a noticeable while in an array of a few
thousand elements."
  (let ((low 0)
        (high (1- (treesit-node-child-count parent t)))
        (start (treesit-node-start child))
        (position nil))
    (while (<= low high)
      (let* ((middle (/ (+ low high) 2))
             (at (treesit-node-start (treesit-node-child parent middle t))))
        (cond ((= at start) (setq position middle
                                  low (1+ high)))
              ((< at start) (setq low (1+ middle)))
              (t (setq high (1- middle))))))
    position))

(defun json-path-eldoc-path-at (pos)
  "Return the path to the JSON value at POS, or nil when there is none.
The path looks like `items[2].name'."
  (when-let* ((parser (json-path-eldoc--parser))
              (node (treesit-node-at pos parser)))
    (let ((child nil)
          (steps nil))
      (while node
        (pcase (treesit-node-type node)
          ("pair" (when-let* ((step (json-path-eldoc--key-step
                                     (treesit-node-child-by-field-name node "key"))))
                    (push step steps)))
          ("array"
           ;; the node came up from is the element, except on a comma,
           ;; where it is the element point just moved past, and on a
           ;; bracket, where point is at the array itself
           (when-let* ((element (cond ((null child) nil)
                                      ((treesit-node-check child 'named) child)
                                      ((equal (treesit-node-type child) ",")
                                       (treesit-node-prev-sibling child t))))
                       (index (json-path-eldoc--child-position node element)))
             (push (format "[%d]" index) steps))))
        (setq child node
              node (treesit-node-parent node)))
      (when steps
        (let ((path (apply #'concat steps)))
          (if json-path-eldoc-leading-dot
              (if (string-prefix-p "." path) path (concat "." path))
            (string-remove-prefix "." path)))))))

;;;###autoload
(defun json-path-eldoc-path ()
  "Show the path to the JSON value point is in, and copy it.
Returns the path when called from Lisp instead."
  (interactive)
  (let ((path (json-path-eldoc-path-at (point))))
    (cond ((not (called-interactively-p 'interactive)) path)
          (path (kill-new path)
                (message "%s" path))
          (t (message "No JSON path at point")))))

(defun json-path-eldoc-function (callback &rest _ignored)
  "Hand the JSON path at point to eldoc's CALLBACK.
Meant for `eldoc-documentation-functions'."
  (when-let* ((path (ignore-errors (json-path-eldoc-path-at (point)))))
    (funcall callback path)
    t))

;;;###autoload
(define-minor-mode json-path-eldoc-mode
  "Show the path to the JSON value point is in in the eldoc echo area."
  :lighter nil
  (if json-path-eldoc-mode
      (progn
        (json-path-eldoc--parser)
        (add-hook 'eldoc-documentation-functions #'json-path-eldoc-function nil t)
        (eldoc-mode 1))
    (remove-hook 'eldoc-documentation-functions #'json-path-eldoc-function t)))

(provide 'json-path-eldoc)

;;; json-path-eldoc.el ends here
