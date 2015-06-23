(setq initial-frame-alist
      '((top . 100) (left . 350) (width . 80) (height . 30)))

;; highlight active line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#222")

;; show matching parens
(show-paren-mode 1)

;; update screen immediately
(setq redisplay-dont-pause t)

;; dont blink cursor
(blink-cursor-mode -1)

;; no bell
(setq ring-bell-function 'ignore)

;; visible bell
(setq visible-bell t)

;; show keystrokes
(setq echo-keystrokes 0.1)

;; gaudiest possible look
(setq font-lock-maximum-decoration t)

;; show active region
(transient-mark-mode 0)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; no splash
(setq inhibit-startup-message t)

;; show empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; full path in titlebar
(setq-default frame-title-format "%b (%f)")

;; rainbow parens
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; small modeline
(require 'diminish)
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")

(provide 'appearance)
