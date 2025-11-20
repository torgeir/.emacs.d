;;; vrl-mode.el --- Major mode for Vector Remap Language code -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Keywords: VRL major mode
;; Author: Stephen Wakely
;; Package-Requires: ((emacs "26.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides syntax highlighting

;;; Code:

(defvar vrl-regexp-regexp
  "/\\(\\(?:\\\\/\\|[^/\n\r]\\)*\\)/")

(defvar vrl-keywords-regexp
  (concat "\\(?:^\\|[^.]\\)"
          (regexp-opt '("if" "true" "false") 'symbols)))

(defvar vrl-path-regexp "\\b\\.\\w*")

(defvar vrl-coalesce-regexp "\\s-\\?\\?\\s-")

(defvar vrl-arg-name "\\b\\sw*?:")

(defvar vrl-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?\# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\. "w" table)

    table))

(defvar vrl-font-lock-keywords
  `((,vrl-regexp-regexp . font-lock-constant-face)
    (,vrl-path-regexp . font-lock-variable-name-face)
    (,vrl-keywords-regexp . font-lock-keyword-face)
    (,vrl-coalesce-regexp . font-lock-builtin-face)
    (,vrl-arg-name . font-lock-comment-face)))

(define-derived-mode vrl-mode prog-mode "VRL"
  "Major mode for editing Vector Remap Language"
  (setq font-lock-defaults '((vrl-font-lock-keywords)))
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq indent-line-function 'indent-relative))

(add-to-list 'auto-mode-alist '("\\.vrl\\'" . vrl-mode))

(provide 'vrl-mode)

