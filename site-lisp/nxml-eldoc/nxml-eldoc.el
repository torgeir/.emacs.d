;;; nxml-eldoc.el --- Show the XML element path at point in eldoc -*- lexical-binding: t; -*-

;; Author: @torgeir
;; Version: 2.0.0
;; Keywords: xml nxml eldoc tree-sitter tools languages
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Show the path to the XML element point is in, as `/root/items/item[2]',
;; in the eldoc echo area.
;;
;; Turn it on with `nxml-eldoc-mode' in a buffer, or add it to
;; `nxml-mode-hook' to have it on in every XML buffer.  Run
;; `nxml-eldoc-path' to put the path in the echo area and the kill ring.
;;
;; The path is read off a tree-sitter tree, so it is O(depth) no matter
;; how big the file is - the old implementation walked the buffer with
;; `nxml-backward-up-element' from point to point-min, which crawled once
;; a file got large.  The tree-sitter `xml' grammar is used, and parses
;; the buffer incrementally as it is edited, also in `nxml-mode' - the
;; parser is only there to answer this question, `nxml-mode' keeps doing
;; the font locking and the editing.
;;
;; An element that has siblings of the same name is numbered, 1-based, as
;; `item[2]', the way XPath does it.  Set `nxml-eldoc-show-index' to nil
;; to leave the numbers out.  When point is inside an attribute, its name
;; is appended as `@id'.

;;; Code:

(require 'treesit)
(require 'subr-x)

(defgroup nxml-eldoc nil
  "Show the XML element path at point in eldoc."
  :group 'nxml
  :prefix "nxml-eldoc-")

(defcustom nxml-eldoc-show-index t
  "Whether to number elements that have siblings of the same name.
With this on the second `item' of a list shows up as `item[2]'."
  :type 'boolean
  :group 'nxml-eldoc)

(defcustom nxml-eldoc-max-siblings 5000
  "How many siblings to look through when numbering an element.
An element that far into a list is left unnumbered, as the siblings in
front of it have to be counted one by one, and that is the one part of
reading a path that grows with the size of the file.  Set it to nil to
always count."
  :type '(choice (const :tag "No limit" nil) integer)
  :group 'nxml-eldoc)

(defun nxml-eldoc--parser ()
  "Return a tree-sitter parser for XML in the current buffer, or nil.
The parser is created on first use and reused after that."
  (or (car (treesit-parser-list nil 'xml))
      (and (treesit-ready-p 'xml t)
           (treesit-parser-create 'xml))))

(defun nxml-eldoc--tag-name (node)
  "Return the name of the `STag', `ETag' or `EmptyElemTag' NODE."
  (let ((name (treesit-node-child node 0 t)))
    (and name
         (equal (treesit-node-type name) "Name")
         (treesit-node-text name t))))

(defun nxml-eldoc--element-name (element)
  "Return the tag name of the `element' node ELEMENT, or nil."
  (let ((tag (treesit-node-child element 0 t)))
    (and tag
         (member (treesit-node-type tag) '("STag" "EmptyElemTag"))
         (nxml-eldoc--tag-name tag))))

(defun nxml-eldoc--child-position (parent child)
  "Return the position of CHILD among PARENT's named children.
The children are sorted by buffer position, so the position is found by
bisecting them - walking the siblings instead, the way
`treesit-node-index' does, is what makes a path slow to read in a file
with a long list of elements."
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

(defun nxml-eldoc--element-named-p (node regexp)
  "Whether NODE starts a tag matching REGEXP.
REGEXP is matched against the buffer text the node starts at, as pulling
the name back out of the tree costs a good deal more, and this runs once
per sibling.  Only an element can start with `<name' followed by a
space, a `/' or a `>' - a comment opens with `<!' and a processing
instruction with `<?' - so the type of the node needs no checking."
  (save-excursion
    (goto-char (treesit-node-start node))
    (looking-at-p regexp)))

(defun nxml-eldoc--element-index (element name)
  "Return the 1-based index of ELEMENT among its siblings called NAME.
Nil when ELEMENT is the only one of that name, as there is nothing to
tell apart then, or when it sits further into the list than
`nxml-eldoc-max-siblings'."
  (when-let* ((parent (treesit-node-parent element))
              (position (nxml-eldoc--child-position parent element))
              (limit (or nxml-eldoc-max-siblings most-positive-fixnum))
              ((<= position limit)))
    (let ((regexp (concat "<" (regexp-quote name) "[ \t\r\n/>]"))
          (count (treesit-node-child-count parent t))
          (before 0)
          (index 0))
      (while (< index position)
        (when (nxml-eldoc--element-named-p (treesit-node-child parent index t) regexp)
          (setq before (1+ before)))
        (setq index (1+ index)))
      (cond
       ;; something of the same name in front of it, so it is the next one
       ((> before 0) (1+ before))
       ;; the first one, which only needs a number if another one follows
       (t (let ((next (1+ position))
                (last (min count (+ position limit)))
                (found nil))
            (while (and (not found) (< next last))
              (setq found (nxml-eldoc--element-named-p
                           (treesit-node-child parent next t) regexp)
                    next (1+ next)))
            (and found 1)))))))

(defun nxml-eldoc--element-step (element)
  "Return the path step for the `element' node ELEMENT, or nil.
That is its tag name, numbered as `item[2]' when it has siblings of the
same name and `nxml-eldoc-show-index' is on."
  (when-let* ((name (nxml-eldoc--element-name element)))
    (let ((index (and nxml-eldoc-show-index
                      (nxml-eldoc--element-index element name))))
      (if index (format "%s[%d]" name index) name))))

(defun nxml-eldoc-path-at (pos)
  "Return the path to the XML element at POS, or nil when there is none.
The path looks like `/root/items/item[2]', with `@id' appended when POS
is inside an attribute."
  (when-let* ((parser (nxml-eldoc--parser))
              (node (treesit-node-at pos parser)))
    (let ((attribute nil)
          (steps nil))
      (while node
        (pcase (treesit-node-type node)
          ("Attribute" (setq attribute (nxml-eldoc--tag-name node)))
          ("element" (when-let* ((step (nxml-eldoc--element-step node)))
                       (push step steps))))
        (setq node (treesit-node-parent node)))
      (when steps
        (concat "/" (mapconcat #'identity steps "/")
                (and attribute (concat "@" attribute)))))))

;;;###autoload
(defun nxml-eldoc-path ()
  "Show the path to the XML element point is in, and copy it.
Returns the path when called from Lisp instead."
  (interactive)
  (let ((path (nxml-eldoc-path-at (point))))
    (cond ((not (called-interactively-p 'interactive)) path)
          (path (kill-new path)
                (message "%s" path))
          (t (message "No XML element at point")))))

(defun nxml-eldoc-function (callback &rest _ignored)
  "Hand the XML path at point to eldoc's CALLBACK.
Meant for `eldoc-documentation-functions'."
  (when-let* ((path (ignore-errors (nxml-eldoc-path-at (point)))))
    (funcall callback path)
    t))

;;;###autoload
(define-minor-mode nxml-eldoc-mode
  "Show the path to the XML element point is in in the eldoc echo area."
  :lighter nil
  (if nxml-eldoc-mode
      (progn
        (nxml-eldoc--parser)
        (add-hook 'eldoc-documentation-functions #'nxml-eldoc-function nil t)
        (eldoc-mode 1))
    (remove-hook 'eldoc-documentation-functions #'nxml-eldoc-function t)))

(provide 'nxml-eldoc)

;;; nxml-eldoc.el ends here
