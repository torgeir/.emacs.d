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
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(require 'expand-region)
(global-set-key (if is-mac (kbd "C-@") (kbd "C-'")) 'er/expand-region)

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

(when (and is-mac
           (not window-system))
  ;; fix terminal shortcomings, remap them in iterm2, and bring them back here
  (define-key input-decode-map "\e[101" (kbd "C-SPC"))
  (define-key input-decode-map "\e[102" (kbd "C-M-SPC"))
  (define-key input-decode-map "\e[103" (kbd "C-."))
  (define-key input-decode-map "\e[104" (kbd "C-,"))
  ;; c-æ on a norwegian mac keyboard IS the ansi escape character ^[
  ;; for debugging run: (read-key-sequence "?")
  (define-key input-decode-map "\e[100" (kbd "C-æ"))
  ;; c-ø on a norwegian mac keyboard is ^\
  (define-key input-decode-map (kbd "C-\\") (kbd "C-ø"))
  ;; c-å on a norwegian mac keyboard is ^]
  (define-key input-decode-map (kbd "C-]") (kbd "C-å")))

;; Mark additional regions matching current region
(global-set-key (kbd "M-æ") 'mc/mark-all-dwim)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
(global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; Symbol and word specific mark-more
                                        ;(global-set-key (kbd "s-æ") 'mc/mark-next-word-like-this)
                                        ;(global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
                                        ;(global-set-key (kbd "M-s-ä") 'mc/mark-all-words-like-this)

(global-set-key (kbd "<C-S-mouse-1>") 'mc/add-cursor-on-click)

;; Experimental multiple-cursors
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; (global-set-key (kbd "s-Æ") 'mc/mark-next-symbol-like-this)
;; (global-set-key (kbd "s-Å") 'mc/mark-previous-symbol-like-this)
;; (global-set-key (kbd "M-s-Æ") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
;; (global-set-key (kbd "C-~") 'mc/reverse-regions)
;; (global-set-key (kbd "M-~") 'mc/sort-regions)
;; (global-set-key (kbd "H-~") 'mc/insert-numbers)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "C-M-SPC") 'set-rectangular-region-anchor)

;; join lines
(global-set-key (kbd "M-j") 'join-lines)

(global-set-key (kbd "C-c r") 'revert-buffer)

(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

(global-set-key (kbd "C-x C-o") 'find-file-in-project)

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
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(provide 'keys)
