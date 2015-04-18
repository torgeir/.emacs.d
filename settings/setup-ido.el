;; turn it on
(require 'ido)
(ido-mode t)

(require 'flx-ido)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; partial matches
(setq ido-enable-flex-matching t)

;; annoying, turn it off
(setq ido-use-filename-at-point nil)

;; only match in the current directory 
(setq ido-auto-merge-work-directories-length -1)

;; show recently open files
(setq ido-use-virtual-buffers t)

;; ido in all contexts
(ido-everywhere 1)

(autoload 'ido-at-point-mode "ido-at-point")
(ido-at-point-mode)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(provide 'setup-ido)
