;;; consult-async.el --- Simplified with consult -*- lexical-binding: t -*-

;; Copyright (C) 2021, 2022  Free Software Foundation, Inc.

;; Author: Torgeir Thoresen
;; Maintainer: Torgeir Thoresen <torgeir.thoresen@gmail.com>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (consult "0.18"))
;; Homepage: nil

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; A simple library for simplified async consult actions with callbacks.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'consult)

(defvar consult-t-history nil)

(defun consult-t-async (&optional fn)
  "Consult spotify by filter."
  (consult--read (consult-t--search-generator fn)
                 :prompt (format "Search: ")
                 ;;:lookup #'consult--lookup-member
                 ;;:category 'spotify-search-item
                 :history '(:input consult-t-history)
                 :initial (consult--async-split-initial "")
                 ;;:require-match t
                 ))

(defun consult-t--search-generator (fn)
  "Generate an async search closure for filter."
  (thread-first (consult--async-sink)
                (consult--async-refresh-immediate)
                (consult--async-map #'(lambda (n) (concat "" n)))
                (consult-t--async-search fn)
                (consult--async-throttle)
                (consult--async-split)))

(defun consult-t--async-search (next fn)
  "Async search with NEXT and FILTER."
  (let ((current ""))
    (lambda (action)
      (pcase action
        ((pred stringp) (if (string-empty-p action)
                            (funcall next 'flush)
                          (when action
                            (funcall next 'flush)
                            (funcall fn action (lambda (res)
                                                 (funcall next (remove-if
                                                                (lambda (n)
                                                                  (or (null n) (string-empty-p n)))
                                                                res)))))))
        (_ (funcall next action))))))

(message "result: %s" (consult-t-async (lambda (action fn)
                                       (message "%s" (type-of fn))
                                       (t/async-shell-command
                                        "file listing"
                                        (concat "ls -l " action)
                                        (lambda (p code res)
                                          (funcall fn res))))))
