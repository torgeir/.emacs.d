;;; -*- lexical-binding: t; -*-
(defun t-mac/vars ()
  ;; trash
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")

  ;; make this run also after connecting with emacsclient
  ;; https://groups.google.com/forum/#!topic/gnu.emacs.help/ZGu2MNkJGrI
  (defadvice terminal-init-xterm (after map-S-up-escape-sequence activate)
    (t/bind-in 'input-decode-map
               ;; fix terminal shortcomings, remap them in iterm2, and bring tem back here
               ;; unused keys are e.g. above f17 which is ^[[15;2~ in emacs that is \e[15;2\~
               ;; http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
               "\e[15;2\~" "C-SPC"
               "\e[17;2\~" "C-M-SPC"
               "\e[18;2\~" "C-."
               "\e[19;2\~" "C-,"
               ;; c-æ on a norwegian mac keyboard IS the ansi escape character ^[
               ;; for debugging run: (read-key-sequence "?")
               "\e[20;2\~" "C-æ"
               ;; c-ø on a norwegian mac keyboard is ^\
               "C-\\" "C-ø"
               ;; c-å on a norwegian mac keyboard is ^]
               "C-]" "C-å"
               ;; skip \e21;2~, its f10? what
               "\e[22;2\~" "C-'"))

  ;; bind fn to H-
  (setq ns-function-modifier 'hyper))

(t/use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(defun t-mac/config ()
  (t/bind-in 'key-translation-map
             ;; translate norwegian os x keybindings
             "M-7" "|"
             "M-/" "\\"
             "M-8" "["
             "M-9" "]"
             "M-(" "{"
             "M-)" "}")

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

  ;; s-w quits like C-x C-w
  (bind-key "s-w" #'t/delete-frame-or-hide-last-remaining-frame)

  ;; don't pop up font menu, makes new tab work in iterm2
  (bind-key "s-t" (t/lambda-i))

  ;; buffer font size adjustment
  (bind-key "s-?" (t/lambda-i (text-scale-increase 1)))
  (bind-key "s-_" (t/lambda-i (text-scale-decrease 1)))
  (bind-key "s-=" (t/lambda-i (text-scale-set 0)))

  ;; global font size adjustment
  (bind-key "s-+" 't/increase-font-size)
  (bind-key "s--" 't/decrease-font-size)
  (bind-key "s-0" 't/reset-font-size)

  ;; s-p print dialog kills emacs, so disable it..
  (bind-key "s-p" (t/lambda-i)))

(provide 't-mac)
