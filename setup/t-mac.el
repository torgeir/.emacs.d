;; make this run also after connecting with emacsclient
;; https://groups.google.com/forum/#!topic/gnu.emacs.help/ZGu2MNkJGrI
(defadvice terminal-init-xterm (after map-S-up-escape-sequence activate)
  (progn
    ;; fix terminal shortcomings, remap them in iterm2, and bring tem back here
    ;; unused keys are e.g. above f17 which is ^[[15;2~ in emacs that is \e[15;2\~
    ;; http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
    (bind-key "\e[15;2\~" "C-SPC" input-decode-map)
    (bind-key "\e[17;2\~" "C-M-SPC" input-decode-map)
    (bind-key "\e[18;2\~" "C-." input-decode-map)
    (bind-key "\e[19;2\~" "C-," input-decode-map)
    ;; c-æ on a norwegian mac keyboard IS the ansi escape character ^[
    ;; for debugging run: (read-key-sequence "?")
    (bind-key "\e[20;2\~" "C-æ" input-decode-map)
    ;; c-ø on a norwegian mac keyboard is ^\
    (bind-key "C-\\" "C-ø" input-decode-map)
    ;; c-å on a norwegian mac keyboard is ^]
    (bind-key "C-]" "C-å" input-decode-map)
    ;; skip \e21;2~, its f10? what
    (bind-key "\e[22;2\~" "C-'" input-decode-map)))

;; bind fn to H-
(setq ns-function-modifier 'hyper)

;; fix os x keybindings
(bind-key "M-7" "|" key-translation-map)
(bind-key "M-/" "\\" key-translation-map)
(bind-key "M-8" "[" key-translation-map)
(bind-key "M-9" "]" key-translation-map)
(bind-key "M-(" "{" key-translation-map)
(bind-key "M-)" "}" key-translation-map)

;; paredit specials
(bind-key "s-(" 'paredit-wrap-round key-translation-map)
(bind-key "s-)" 't/paredit-wrap-round-from-behind key-translation-map)
(bind-key "M-s-(" 'paredit-wrap-curly key-translation-map)
(bind-key "M-s-[" 'paredit-wrap-square key-translation-map)

;; os x window movement
(bind-key "s->" 'next-multiframe-window)
(bind-key "s-<" 'previous-multiframe-window)
(bind-key "s-<left>" 't/smart-beginning-of-line)
(bind-key "s-<right>" 'end-of-line)
(bind-key "M-s-<up>" 'windmove-up)
(bind-key "M-s-<right>" 'windmove-right)
(bind-key "M-s-<down>" 'windmove-down)
(bind-key "M-s-<left>" 'windmove-left)
(bind-key "s-d" 't/split-window-right-and-move-there-dammit)
(bind-key "s-D" 't/split-window-below-and-move-there-dammit)
(bind-key "C-s-<right>" 't/increase-frame-width)
(bind-key "C-s-<left>" 't/decrease-frame-width)
(bind-key "C-s-<down>" 't/increase-frame-height)
(bind-key "C-s-<up>" 't/decrease-frame-height)
(bind-key "C-s-S-<right>" 't/move-frame-right)
(bind-key "C-s-S-<left>" 't/move-frame-left)
(bind-key "C-s-S-<down>" 't/move-frame-down)
(bind-key "C-s-S-<up>" 't/move-frame-up)

;; s-w quits like C-x C-w
(bind-key "s-w" #'t/delete-frame-or-hide-last-remaining-frame)

;; don't pop up font menu, makes new tab work in iterm2
(bind-key "s-t" '(lambda () (interactive)))

;; buffer font size adjustment
(bind-key "s-?" '(lambda () (interactive) (text-scale-increase 1)))
(bind-key "s-_" '(lambda () (interactive) (text-scale-decrease 1)))
(bind-key "s-=" '(lambda () (interactive) (text-scale-set 0)))

;; global font size adjustment
(bind-key "s-+" 't/increase-font-size)
(bind-key "s--" 't/decrease-font-size)
(bind-key "s-0" 't/reset-font-size)

;; trash
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

(setq t/initial-font-size 17)

;; For when Emacs is started in GUI mode:
(t/set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions 't/set-emoji-font)

(provide 't-mac)
