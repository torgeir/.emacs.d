(setq user-full-name (getenv "USER_FULLNAME")
      user-mail-address (concat (getenv "USER") "@gmail.com")
      gnus-posting-styles '((".*"
                             (signature "T"))))

(setq gnus-activate-level 3
      gnus-use-cache t
      gnus-level-subscribed 3
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nnhackernews "")
        (nnreddit "")
        (nnimap "gmail"
                ;; it could also be imap.googlemail.com if that's your server.
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "imaps")
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo.gpg")
                (nnir-search-engine imap)
                ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                ;; press 'E' to expire email
                (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                (nnmail-expiry-wait 90))))

;; https://sachachua.com/blog/2008/05/emacs-gnus-organize-your-mail/
(setq nnimap-split-inbox "INBOX")
(setq nnimap-split-predicate "UNDELETED")
(setq nnimap-split-methods
      '(("INBOX.saga" "^Subject:.*saga.*")
        ("INBOX.github" "^Subject:.*github.*")
        ("INBOX.fun" "^From:.*@bikeshop.no")
        ("INBOX.other" "")))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; Make Gnus NOT ignore [Gmail] mailboxes
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-registry-max-entries 2500
      gnus-registry-track-extra '(sender subject recipient))

(gnus-registry-initialize)

(require 'epa-file)
(setq epg-gpg-program "/usr/local/bin/gpg"
      ;; ask only once
      epa-file-cache-passphrase-for-symmetric-encryption t)
(epa-file-enable)

(defun t-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)g
  (gnus-group-list-all-groups 3))

(evil-define-key 'emacs gnus-group-mode-map
  (kbd "o") 't-gnus-group-list-subscribed-groups)

(evil-define-key 'emacs gnus-group-mode-map
  (kbd "<tab>")
  'gnus-topic-select-group)

(setq smiley-style 'large
      gnus-mime-display-multipart-related-as-mixed t)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; (evil-collection-gnus-setup)

(evil-define-key 'emacs gnus-summary-mode-map
  ;; motion
  (kbd "p") 'gnus-summary-prev-page
  (kbd "n") 'gnus-summary-next-page)

;; (t/add-hook-defun 'gnus-summary-mode-hook t-gnus-summary-hook
;;                   (evil-leader-mode -1))

;; (t/add-hook-defun 'gnus-exit-gnus-hook t-gnus-exit-hook
;;                   (evil-leader-mode 1))