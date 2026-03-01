(package! centered-window-mode
  :pin "701f56cd1d2b68352d29914f05ca1b0037bb2595"
  :recipe (:host github :repo "nullvec/centered-window-mode" :files ("centered-window.el")))
(package! nerd-icons-dired)
(package! d2-mode)
(package! llm)
(package! ellama)
(package! pulsar)
(package! chatgpt-shell)
(package! agent-shell)
(package! shell-maker)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! tide :disable t)
(package! catppuccin-theme)
(package! elfeed-tube)
(package! evil-cleverparens)
(package! transpose-frame)
(package! highlight-symbol)
(package! olivetti)
(package! calendar-norway)
(package! dired-subtree)
(package! mastodon :recipe (:host codeberg :repo "martianh/mastodon.el"))
(package! hnreader)
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! remark-mode)
(package! command-log-mode) ;; C-c o
;; needed for wn program on a mac?
(package! wordnut :pin "feac531404041855312c1a046bde7ea18c674915")
;; needed for wn program on a mac?
(package! synosaurus :pin "14d34fc92a77c3a916b4d58400424c44ae99cd81")
(package! recursion-indicator)
(package! org-alert)
(package! spacious-padding)
(package! mu4e-alert)
(package! rainbow-delimiters)

;; Default branch is main.
(package! iedit :recipe (:branch "main"))
