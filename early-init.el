;;; early-init.el --- Early init -*- lexical-binding: t; -*-

;; avoid package.el activation since packages are managed by t-package,
;; prevents errors during startup about packages missing - that are present
(setq package-enable-at-startup nil)

;; temporarily raise gc limits during startup, restore after.
(defvar t--backup-gc-cons-threshold gc-cons-threshold)
(defvar t--backup-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold t--backup-gc-cons-threshold)
            (setq gc-cons-percentage t--backup-gc-cons-percentage))
          100)

;; reduce file-name handler overhead during startup, restore after.
(defvar t--old-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist t--old-file-name-handler-alist)))

;; improve throughput for subprocess i/o (lsp, ripgrep, git).
(setq read-process-output-max (* 2 1024 1024))
