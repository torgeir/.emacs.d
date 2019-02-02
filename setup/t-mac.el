;;; -*- lexical-binding: t; -*-
(defun t-mac/init ()

  (t/use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize))

  ;; mouse
  (setq ns-use-mwheel-momentum t
        ns-use-mwheel-acceleration t

        ;; bind fn to H-
        ns-function-modifier 'hyper

        trash-directory "~/.Trash/emacs")

  ;; dark title bar
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (t/bind-in 'key-translation-map
    ;; translate norwegian os x keybindings
    "M-7" "|"
    "M-/" "\\"
    "M-8" "["
    "M-9" "]"
    "M-(" "{"
    "M-)" "}")

  (t/bind-in 'global-map
    ;; s-p print dialog kills emacs, so disable it..
    "s-p" nil
    ;; don't pop up font menu, makes new tab work in iterm2
    "s-t" nil)

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
      "\e[22;2\~" "C-'")))

(provide 't-mac)
