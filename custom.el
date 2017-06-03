(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-checkbox-statistics-todo ((t (:inherit org-todo))))
 '(org-todo ((t (:background "#32322c" :foreground "#dc752f" :weight bold))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#e0211d" :overline nil :underline t)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(custom-safe-themes
   (quote
    ("666c783d1534051189c9bca391037fc5a11dbc5d51dbe80e8148d66bfa4e9fdb" "ef479623c75026d8ba1de98a8cb74198f6f3eedc6fca509990ac2559ba830675" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "d320493111089afba1563bc3962d8ea1117dd2b3abb189aeebdc8c51b5517ddb" "eb0a314ac9f75a2bf6ed53563b5d28b563eeba938f8433f6d1db781a47da1366" "8ec2e01474ad56ee33bc0534bdbe7842eea74dccfb576e09f99ef89a705f5501" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "0eea76fe89061a7f6da195f4a976c0b91150de987b942fac2dd10992aea33833" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "badc4f9ae3ee82a5ca711f3fd48c3f49ebe20e6303bba1912d4e2d19dd60ec98" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5d1434865473463d79ee0523c1ae60ecb731ab8d134a2e6f25c17a2b497dd459" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#3E4451" t)
 '(flycheck-javascript-flow-args nil)
 '(helm-source-names-using-follow (quote ("Show hunks in project")))
 '(magit-pull-arguments nil t)
 '(package-selected-packages
   (quote
    (evil-commentary yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode company-anaconda anaconda-mode pythonic skewer-mode simple-httpd haml-mode anzu highlight bind-map packed auto-complete clojure-snippets clj-refactor inflections edn peg cider-eval-sexp-fu cider queue clojure-mode mmm-mode markdown-toc markdown-mode gh-md nodejs-repl alert log4e gntp noflet elfeed visual-fill-column wgrep hide-lines loop nlinum google makey restclient know-your-http-well flycheck-pos-tip pos-tip flycheck vi-tilde-fringe ws-butler writeroom-mode winum which-key wgrep-ag web-mode web-beautify weather-metno w3m volatile-highlights visual-regexp uuidgen use-package try transpose-frame toc-org tagedit syslog-mode suggest spaceline smex slim-mode selectric-mode scss-mode sass-mode restart-emacs request rainbow-mode rainbow-delimiters pug-mode persp-mode pcre2el paradox org-mac-link org-mac-iCal org-evil org-bullets org-alert open-junk-file ob-restclient nlinum-relative neotree move-text magit-gh-pulls macrostep lorem-ipsum livid-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc info+ indent-guide hungry-delete hl-todo highlight-symbol highlight-parentheses highlight-numbers highlight-indentation highlight-escape-sequences hide-comnt help-fns+ helm-unicode helm-themes helm-swoop helm-projectile helm-open-github helm-mode-manager helm-make helm-hunks helm-google helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag hackernews gruvbox-theme google-translate golden-ratio git-timemachine git-link git-gutter-fringe+ gist fuzzy flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-multiedit evil-mc evil-matchit evil-lisp-state evil-leader evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu etags-select eshell-z emmet-mode elisp-slime-nav elfeed-org elfeed-goodies dumb-jump doom-themes discover-my-major dired-details define-word dash-at-point darktooth-theme csv-mode company-web company-tern company-statistics company-restclient company-flx company-emoji company-ansible column-enforce-mode coffee-mode clean-aindent-mode calendar-norway better-defaults bash-completion auto-yasnippet auto-highlight-symbol auto-compile all-the-icons-dired aggressive-indent ag adaptive-wrap ace-window ace-link ace-jump-helm-line ac-ispell)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(safe-local-variable-values
   (quote
    ((eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))))))
