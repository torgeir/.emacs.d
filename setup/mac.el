;; make this run also after connecting with emacsclient
;; https://groups.google.com/forum/#!topic/gnu.emacs.help/ZGu2MNkJGrI
(when is-mac
  (defadvice terminal-init-xterm (after map-S-up-escape-sequence activate)
    (progn
      ;; fix terminal shortcomings, remap them in iterm2, and bring tem back here
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
      (define-key input-decode-map (kbd "C-]") (kbd "C-å"))
      ;; skip \e21;2~, its f10? what
      (define-key input-decode-map "\e[22;2\~" (kbd "C-'")))))

;; bind fn to H-
(setq ns-function-modifier 'hyper)

;; fix os x keybindings
(define-key key-translation-map (kbd "M-7") (kbd "|"))
(define-key key-translation-map (kbd "M-/") (kbd "\\"))
(define-key key-translation-map (kbd "M-8") (kbd "["))
(define-key key-translation-map (kbd "M-9") (kbd "]"))
(define-key key-translation-map (kbd "M-(") (kbd "{"))
(define-key key-translation-map (kbd "M-)") (kbd "}"))

;; paredit specials
(define-key key-translation-map (kbd "s-(") 'paredit-wrap-round)
(define-key key-translation-map (kbd "s-)") 't/paredit-wrap-round-from-behind)
(define-key key-translation-map (kbd "M-s-(") 'paredit-wrap-curly)
(define-key key-translation-map (kbd "M-s-[") 'paredit-wrap-square)

;; ;; os x window movement
(global-set-key (kbd "s->") 'next-multiframe-window)
(global-set-key (kbd "s-<") 'previous-multiframe-window)
(global-set-key (kbd "s-<left>") 't/smart-beginning-of-line)
(global-set-key (kbd "s-<right>") 'end-of-line)
(global-set-key (kbd "M-s-<up>") 'windmove-up)
(global-set-key (kbd "M-s-<right>") 'windmove-right)
(global-set-key (kbd "M-s-<down>") 'windmove-down)
(global-set-key (kbd "M-s-<left>") 'windmove-left)
(global-set-key (kbd "s-d") 't/split-window-right-and-move-there-dammit)
(global-set-key (kbd "s-D") 't/split-window-below-and-move-there-dammit)
(global-set-key (kbd "C-s-<right>") 't/increase-frame-width)
(global-set-key (kbd "C-s-<left>") 't/decrease-frame-width)
(global-set-key (kbd "C-s-<down>") 't/increase-frame-height)
(global-set-key (kbd "C-s-<up>") 't/decrease-frame-height)

;; don't pop up font menu, makes new tab work in iterm2
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; buffer font size adjustment
(global-set-key (kbd "s-?") '(lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "s-_") '(lambda () (interactive) (text-scale-decrease 1)))
(global-set-key (kbd "s-=") '(lambda () (interactive) (text-scale-set 0)))

;; global font size adjustment
(global-set-key (kbd "s-+") 't/increase-font-size)
(global-set-key (kbd "s--") 't/decrease-font-size)
(global-set-key (kbd "s-0") 't/reset-font-size)

;; trash
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

(setq t/initial-font-size 14)

(defun t/--set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly 🚀"
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") frame 'prepend)
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
;; For when Emacs is started in GUI mode:
(t/--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions 't/--set-emoji-font)

(provide 'mac)
