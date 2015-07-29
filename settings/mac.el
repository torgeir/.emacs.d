;; bind fn to H-
(setq ns-function-modifier 'hyper)

;; fix os x keybindings
(define-key key-translation-map (kbd "M-8") (kbd "["))
(define-key key-translation-map (kbd "M-(") (kbd "{"))
(define-key key-translation-map (kbd "M-9") (kbd "]"))
(define-key key-translation-map (kbd "M-)") (kbd "}"))
(define-key key-translation-map (kbd "M-7") (kbd "|"))
(define-key key-translation-map (kbd "M-/") (kbd "\\"))

;; paredit specials
(define-key key-translation-map (kbd "s-(") 'paredit-wrap-round)
(define-key key-translation-map (kbd "s-)") 'paredit-wrap-round-from-behind)
(define-key key-translation-map (kbd "M-s-(") 'paredit-wrap-curly)
(define-key key-translation-map (kbd "M-s-[") 'paredit-wrap-square)

;; mac friendly font
(when window-system
  (setq torgeir/default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (setq torgeir/presentation-font "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font torgeir/default-font))

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

;; env vars from user shell
(require-package 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; clipboard
(defun copy-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'copy-to-osx)

;; (defun paste-from-osx ()
;;   (shell-command-to-string "pbpaste"))
;; (setq interprogram-paste-function 'paste-from-osx)

(provide 'mac)
