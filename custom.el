(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-background ((t (:background "#21242b"))))
 '(dictionary-word-definition-face ((t nil)))
 '(eshell-ls-executable ((t (:inherit doom-modeline-info :weight bold))))
 '(evil-goggles-delete-face ((t (:inherit magit-diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit magit-diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit magit-diff-added))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit magit-diff-removed))))
 '(font-lock-builtin-face ((t (:foreground "#e678dd"))))
 '(font-lock-comment-face ((t (:foreground "#5a9fc0"))))
 '(font-lock-constant-face ((t (:foreground "#baa1e1"))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "gray60"))))
 '(font-lock-function-name-face ((t (:foreground "#ff78dd"))))
 '(font-lock-keyword-face ((t (:foreground "#31afff"))))
 '(font-lock-string-face ((t (:foreground "#98da65"))))
 '(font-lock-type-face ((t (:foreground "#febf7B"))))
 '(font-lock-variable-name-face ((t (:foreground "#ecaeff"))))
 '(hackernews-comment-count-face ((t (:inherit org-code))) t)
 '(hackernews-link-face ((t (:inherit font-lock-keyword-face))) t)
 '(helm-candidate-number ((t nil)))
 '(highlight-numbers-number ((t (:inherit bold :foreground "#ff9548"))))
 '(highlight-symbol-face ((t (:inherit hl-line))))
 '(holiday ((t (:foreground "OliveDrab3"))))
 '(js2-external-variable ((t nil)))
 '(lazy-highlight ((t (:background "#2266A0" :foreground "#DFDFDF" :weight bold))))
 '(lsp-face-highlight-read ((t (:background "#222" :foreground "#DFDFDF" :weight normal))))
 '(markdown-header-face ((t (:inherit bold :foreground "#66bb5c"))))
 '(org-agenda-dimmed-todo-face ((t (:inherit org-level-2))))
 '(org-checkbox-statistics-todo ((t (:inherit org-todo))))
 '(org-code ((t (:foreground "VioletRed1"))))
 '(org-date ((t (:foreground "turquoise2"))))
 '(org-done ((t (:strike-through nil))))
 '(org-headline-done ((t (:strike-through t))))
 '(org-level-1 ((t (:foreground "#51afef" :weight semi-bold :height 1.0 :background nil))))
 '(org-scheduled-previously ((t (:inherit error))))
 '(org-special-keyword ((t (:foreground "gray25"))))
 '(org-tag ((t (:foreground "#7bda75" :weight normal))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#e0211d" :overline nil :underline t))))
 '(spray-accent-face ((t (:inherit spray-base-face :foreground "#51afef")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(company-box-doc-delay 0.01)
 '(custom-safe-themes
   '("f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "5715d3b4b071d33af95e9ded99a450aad674e308abb06442a094652a33507cd2" "c5d320f0b5b354b2be511882fc90def1d32ac5d38cccc8c68eab60a62d1621f2" "1219caa012a72ee96a86bba91fd6ec4eca2586dbcd1fe82facb5d0655a28b055" "3a651bfd6708cd2995c6f9b50146e890e06419d445980a7cdc095af245899aa7" "666c783d1534051189c9bca391037fc5a11dbc5d51dbe80e8148d66bfa4e9fdb" "ef479623c75026d8ba1de98a8cb74198f6f3eedc6fca509990ac2559ba830675" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "0eea76fe89061a7f6da195f4a976c0b91150de987b942fac2dd10992aea33833" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" default))
 '(flycheck-javascript-flow-args nil)
 '(json-reformat:indent-width 2 t)
 '(org-agenda-files
   '("/Users/torgeirthoresen/Dropbox/org/tasks.org" "/Users/torgeirthoresen/Dropbox/org/prog.org" "/Users/torgeirthoresen/Dropbox/org/org-tricks.org" "/Users/torgeirthoresen/Dropbox/org/ip.org" "/Users/torgeirthoresen/Dropbox/org/info.org" "/Users/torgeirthoresen/Dropbox/org/inbox.org" "/Users/torgeirthoresen/Dropbox/org/home.org" "/Users/torgeirthoresen/Dropbox/org/heart-of-clojure.org" "/Users/torgeirthoresen/Dropbox/org/gcal/delt.org" "/Users/torgeirthoresen/Dropbox/org/feeds.org" "/Users/torgeirthoresen/Dropbox/org/diary.org" "/Users/torgeirthoresen/Dropbox/org/bekk/bekk.org" "/Users/torgeirthoresen/Dropbox/org/bekk/datainn.org" "/Users/torgeirthoresen/Dropbox/org/IFTTT/pocket.txt" "/Users/torgeirthoresen/Dropbox/org/IFTTT/twitter.txt"))
 '(org-agenda-window-setup 'only-window t)
 '(package-selected-packages
   '(lsp-java java-mode md4rd lsp-typescript lsp-javascript-typescript helm-lsp lsp-ui lsp-mode swiper-helm org-gcal esh-autosuggest synosaurus dictionary benchmark-init request-deferred forge company-box doom-modeline diminish magit-gh-pulls rjsx-mode twittering twittering-mode evil-collection fold-this nav-flash evil-goggles rsjx-mode php-mode applescript-mode es-mode fsharp-mode cdnjs helm-js-codemod js-codemod flow-minor-mode fireplace dired-subtree artist-mode company-tern company-quickhelp company-terraform terraform-mode pcomplete-extension pcomplete-extensions pcmpl-args pcmpl-homebrew pcmpl-git indium esh-help etags-table prettier-js org-plus-contrib smooth-scrolling yasnippet emoji-cheat-sheet-plus company ace-jump-mode magit git-gutter+ solaire-mode spacemacs-theme evil all-the-icons evil-snipe html2text helm-xref helm-hunks eldoc-eval evil-commentary yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic skewer-mode simple-httpd haml-mode anzu highlight bind-map packed auto-complete clojure-snippets clj-refactor inflections edn peg cider-eval-sexp-fu cider queue clojure-mode mmm-mode markdown-toc markdown-mode gh-md nodejs-repl alert log4e gntp noflet elfeed visual-fill-column wgrep hide-lines loop nlinum google makey restclient know-your-http-well flycheck-pos-tip pos-tip flycheck vi-tilde-fringe ws-butler writeroom-mode winum which-key wgrep-ag web-mode web-beautify weather-metno w3m volatile-highlights visual-regexp uuidgen use-package try transpose-frame toc-org tagedit syslog-mode suggest spaceline smex slim-mode selectric-mode scss-mode sass-mode restart-emacs request rainbow-mode rainbow-delimiters pug-mode persp-mode pcre2el paradox org-mac-link org-mac-iCal org-evil org-bullets org-alert open-junk-file ob-restclient nlinum-relative neotree move-text macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide hungry-delete hl-todo highlight-symbol highlight-parentheses highlight-numbers highlight-indentation highlight-escape-sequences hide-comnt help-fns+ helm-unicode helm-themes helm-swoop helm-projectile helm-open-github helm-mode-manager helm-make helm-google helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hackernews gruvbox-theme google-translate golden-ratio git-timemachine git-link git-gutter-fringe+ gist fuzzy flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-multiedit evil-mc evil-matchit evil-lisp-state evil-leader evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu etags-select eshell-z emmet-mode elisp-slime-nav elfeed-org elfeed-goodies dumb-jump doom-themes discover-my-major dired-details define-word dash-at-point darktooth-theme csv-mode company-web company-statistics company-restclient company-flx company-emoji company-ansible column-enforce-mode coffee-mode clean-aindent-mode calendar-norway better-defaults bash-completion auto-yasnippet auto-highlight-symbol auto-compile all-the-icons-dired aggressive-indent ag adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(safe-local-variable-values
   '((*t-indent* . 4)
     (web-mode-code-indent-offset . 4)
     (web-mode-css-indent-offset . 4)
     (web-mode-markup-indent-offset . 4)))
 '(tags-add-tables t))
