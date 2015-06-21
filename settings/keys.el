(require 'util)
(require 'defuns)

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

(global-set-key (kbd "C-o") 'open-line-below)
(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<s-backspace>") 'kill-whole-line)

(global-set-key (kbd "<C-S-up>") 'move-text-up)
(global-set-key (kbd "<C-S-down>") 'move-text-down)

;; M-g M-g
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "M-i") 'back-to-indentation)

;; ; to go forward and , to go back
(global-set-key [(meta m)] 'jump-char-forward)
(global-set-key [(shift meta m)] 'jump-char-backward)

;; m-p and m-b bindings, and bring them back from various plugins
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(eval-after-load "w3m"
  '(progn
     (define-key w3m-mode-map (kbd "M-n") nil)))

(require 'markdown-mode)
(define-key markdown-mode-map (kbd "M-p") nil)
(define-key markdown-mode-map (kbd "M-n") nil)

(require 'expand-region)
(global-set-key (if is-mac (kbd "C-@") (kbd "C-'")) 'er/expand-region)

(global-set-key (kbd "C-x m") 'magit-status)
(autoload 'magit-status "magit")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; rename and delete file
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; smarter m-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(windmove-default-keybindings)

;; os x window movement
(global-set-key (kbd "s->") 'next-multiframe-window)
(global-set-key (kbd "s-<") 'previous-multiframe-window)

;; whitespace, indent etc
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; make this run also after connecting with emacsclient
;; https://groups.google.com/forum/#!topic/gnu.emacs.help/ZGu2MNkJGrI
(when is-mac
  (defadvice terminal-init-xterm (after map-S-up-escape-sequence activate)
    (progn
      ;; fix terminal shortcomings, remap them in iterm2, and bring them back here
      ;; unused keys are e.g. above f17 which is ^[[15;2~ in emacs that is \e[15;2\~
      ;; http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
      (define-key input-decode-map "\e[15;2\~" (kbd "C-SPC"))
      (define-key input-decode-map "\e[17;2\~" (kbd "C-M-SPC"))
      (define-key input-decode-map "\e[18;2\~" (kbd "C-."))
      (define-key input-decode-map "\e[19;2\~" (kbd "C-,"))
      ;; c-æ on a norwegian mac keyboard IS the ansi escape character ^[
      ;; for debugging run: (read-key-sequence "?")
      (define-key input-decode-map "\e[20;2\~" (kbd "C-æ"))
      ;; c-ø on a norwegian mac keyboard is ^\
      (define-key input-decode-map (kbd "C-\\") (kbd "C-ø"))
      ;; c-å on a norwegian mac keyboard is ^]
      (define-key input-decode-map (kbd "C-]") (kbd "C-å")))))

;; Mark additional regions matching current region
(global-set-key (kbd "M-æ") 'mc/mark-all-dwim)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
(global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-å") 'mc/mark-all-in-region)

(global-set-key (kbd "<C-S-mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)

;; join lines
(global-set-key (kbd "M-j") 'join-lines)

(global-set-key (kbd "C-c r") 'revert-buffer)

(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

(global-set-key (kbd "C-x C-g") 'ffap)

(global-set-key (kbd "C-x C-o") 'find-file-in-project)
(global-set-key (kbd "C-x M-p") 'set-project-root-to-symbol-at-point)
(global-set-key (kbd "C-x M-n") 'new-empty-buffer)

(global-set-key (kbd "C-k") 'kill-and-join-forward)

(add-hook 'sgml-mode-hook
          (lambda ()
            (define-key sgml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair)))

(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
     (define-key html-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
     (define-key html-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
     (define-key html-mode-map (kbd "C-k") 'tagedit-kill)
     (define-key html-mode-map (kbd "M-k") 'tagedit-kill-attribute)
     (define-key html-mode-map (kbd "M-r") 'tagedit-raise-tag)
     (define-key html-mode-map (kbd "M-s") 'tagedit-splice-tag)
     (define-key html-mode-map (kbd "M-S") 'tagedit-split-tag)
     (define-key html-mode-map (kbd "M-J") 'tagedit-join-tags)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(provide 'keys)
