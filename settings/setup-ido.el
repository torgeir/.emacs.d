;; turn it on
(require 'ido)
(ido-mode t)

(require 'flx-ido)
(flx-ido-mode 1)

;; match arbitrary substrings
(setq ido-enable-prefix nil)

;; ignore case when searching for buffers and file names
(setq ido-case-fold t)

;; always new buffers if no buffer matches substring
(setq ido-create-new-buffer 'always)

;; partial matches
(setq ido-enable-flex-matching t)

;; only match in the current directory 
(setq ido-auto-merge-work-directories-length -1)

;; show recently open files
(setq ido-use-virtual-buffers t)

;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

;; ido in all contexts
(ido-everywhere 1)

;; use ido for completion-at-point
(autoload 'ido-at-point-mode "ido-at-point")
(ido-at-point-mode 1)

(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; ignores
(add-to-list 'ido-ignore-files "\\.DS_Store")
(add-to-list 'ido-ignore-directories "target")
(add-to-list 'ido-ignore-directories "node_modules")

(provide 'setup-ido)
