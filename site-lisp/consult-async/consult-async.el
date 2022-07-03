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
(require 'dom)
(require 'request)

(defvar consult-t-history nil)

(defun consult-t-async (&optional fn)
  "Consult spotify by filter."
  (consult--read (consult-t--search-generator fn)
                 :prompt (format "Search: ")
                 ;;:lookup #'consult--lookup-member
                 :state (lambda (action q)
                          (message "action: %s, q: %s" action q)
                          (when (and (not (null q))
                                     (s-starts-with? "http" q))
                            (eww q)
                            ))
                 ;;:category 'spotify-search-item
                 :history '(:input consult-t-history)
                 :initial (consult--async-split-initial "")
                 ;;:require-match t
                 :keymap (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "C-M-S-v") (lambda ()
                                                             (interactive)
                                                             (save-excursion
                                                               (with-selected-window (get-buffer-window "*eww*")
                                                                 (scroll-down)))))
                           (define-key map (kbd "C-M-v") (lambda ()
                                                           (interactive)
                                                           (save-excursion
                                                             (with-selected-window (get-buffer-window "*eww*")
                                                               (scroll-up)))))
                           map)))

(defun consult-t--search-generator (fn)
  "Generate an async search closure for filter."
  (thread-first (consult--async-sink)
                (consult--async-refresh-immediate)
                (consult-t--async-search fn)
                (consult--async-throttle .1 .2)
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
                                                 (funcall next res))))))
        (_ (funcall next action))))))

(defun consult-web--format-candidate (text url)
  "Format TEXT and URL as an `ivy-read' candidate."
  (let ((url (url-unhex-string url)))
    (propertize (concat text "\n" (propertize url 'face 'shadow)) 'shr-url url)))

(cl-defun consult-web--handle-error (&rest args &key error-thrown &allow-other-keys)
  "Handle error from `request' with ARGS.
Display a message with the ERROR-THROWN."
  (error "Web search error: %S" error-thrown))

(defun consult--clear-text-properties (foo)
  (set-text-properties 0 (length foo) nil foo)
  foo)

(defun consult-web--request (fn url parser &optional placeholder)
  (message "searching: %s" url)
  (request
    url
    :sync t
    :headers '(("User-Agent" . "Emacs"))
    :parser parser
    :error #'consult-web--handle-error
    :success (cl-function (lambda (&key data &allow-other-keys)
                            (funcall fn (mapcar
                                         (lambda (e)
                                           (cadr (split-string (consult--clear-text-properties e) "\n")))
                                         (cddr data)))))))

(defun consult-web-search--ddg (string fn)
  "Retrieve search results from DuckDuckGo for STRING."
  (consult-web--request
   fn
   (concat "https://duckduckgo.com/html/?q=" (url-hexify-string string))
   (lambda ()
     (mapcar
      (lambda (a)
        (let* ((href (assoc-default 'href (dom-attributes a))))
          (consult-web--format-candidate
           (dom-texts a)
           ;; DDG sometimes appends "&rut...", which I can only guess is an
           ;; anti-bot measure. See https://github.com/mnewt/counsel-web/issues/3.
           (substring href (string-match "http" href) (string-match "&rut=" href)))))
      (dom-by-class (libxml-parse-html-region (point-min) (point-max)) "result__a")))
   "Searching DuckDuckGo..."))

(defun consult-web-search ()
  (interactive)
  (consult-t-async (lambda (q fn)
                     (consult-web-search--ddg q fn))))


(progn
  (defvar-local consult-toggle-preview-orig nil)

  (defun consult-toggle-preview ()
    "Command to enable/disable preview."
    (interactive)
    (if consult-toggle-preview-orig
        (setq consult--preview-function consult-toggle-preview-orig
              consult-toggle-preview-orig nil)
      (setq consult-toggle-preview-orig consult--preview-function
            consult--preview-function #'ignore)))

  ;; Bind to `vertico-map' or `selectrum-minibuffer-map'
  (after! vertico
    (define-key vertico-map (kbd "M-p") #'consult-toggle-preview))

  )

(provide 'consult-async)
