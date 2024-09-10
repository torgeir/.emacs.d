(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("a060c0315a44bee19ac87414571131c8c8fad7a940bda0102da67410e66833aa" "c95f1add64292a11748891ea78c5c234afe74d9f1e326cbc20e5f517c048f770" "c38ca564fb26ae0414ed076b9a8462cdfbb1e20eb651001bfaa789e842fdbfdd" "0527c20293f587f79fc1544a2472c8171abcc0fa767074a0d3ebac74793ab117" "a9eeab09d61fef94084a95f82557e147d9630fbbb82a837f971f83e66e21e5ad" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "7cf720b5ecb20f43fbeef85380a5a7c44cfff091aa8c1b0265d0297ab3bd5d3f" "1c698719e9d4a63a25e97c59db57d104845b93152002bba2642fe4a850e931c4" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" "6d0be7d58f4169e34ea0f3aad456f0e4f8414430c612efe66ec6739ad5ee5e26" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path 'file)
 '(safe-local-variable-values
   '((eval progn
      (after! forge
        (add-to-list 'forge-alist
                     '("personal" "api.github.com" "github.com" forge-github-repository))
        (add-to-list 'forge-alist
                     '("work" "api.github.com" "github.com" forge-github-repository)))
      (after! browse-at-remote
        (add-to-list 'browse-at-remote-remote-type-regexps
                     '(:host "^personal$" :type "github" :actual-host "github.com"))
        (add-to-list 'browse-at-remote-remote-type-regexps
                     '(:host "^work$" :type "github" :actual-host "github.com")))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-refine-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-refine-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-refine-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-refine-changed))))
 '(fringe ((t :background "#251D2F")))
 '(header-line ((t :box (:line-width 4 :color "#17121d" :style nil))))
 '(header-line-highlight ((t :box (:color "#c4b8d3"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#251D2F")))
 '(mode-line ((t :box (:line-width 6 :color "#17121d" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#17121d" :style nil))))
 '(mode-line-highlight ((t :box (:color "#c4b8d3"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#1A1521" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#251D2F" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#1A1521" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-current ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#251D2F" :foreground "#251D2F")))
 '(window-divider ((t (:background "#251D2F" :foreground "#251D2F"))))
 '(window-divider-first-pixel ((t (:background "#251D2F" :foreground "#251D2F"))))
 '(window-divider-last-pixel ((t (:background "#251D2F" :foreground "#251D2F")))))
