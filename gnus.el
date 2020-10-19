(when (not (getenv "USER_FULLNAME")) (message "Need env USER_FULLNAME "))
(when (not (getenv "USER")) (message "Need env USER "))

(setq user-full-name (getenv "USER_FULLNAME")
      user-mail-address (downcase (concat (replace-regexp-in-string
                                           " " "."
                                           (getenv "USER_FULLNAME")) "@gmail.com"))
      gnus-posting-styles '((".*" (signature "T"))))

(darkroom-mode -1)
(darkroom-tentative-mode -1)

(setq gnus-activate-level 3
      gnus-use-cache t
      gnus-level-subscribed 3
      gnus-select-method '(nnnil "")
      gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nntp "news.gwene.org")
        (nnimap "gmail"
                ;; it could also be imap.googlemail.com if that's your server.
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port "imaps")
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo.gpg")
                (nnir-search-engine imap)
                ;; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
                ;; press 'E' to eire email
                (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
                (nnmail-expiry-wait 90)
                ;; https://sachachua.com/blog/2008/05/emacs-gnus-organize-your-mail/
                )))


(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      ;; Make Gnus NOT ignore [Gmail] mailboxes
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq gnus-registry-max-entries 2500
      gnus-registry-track-extra '(sender subject recipient))

(gnus-registry-initialize)

;; use emacs prompt for gpg pass phrase
;;(setenv "GPG_AGENT_INFO" nil)

;; -- ~/.authinfo.gpg --
;; machine imap.gmail.com login torgeir.thoresen@gmail.com port 993 password <pwd>
;; machine smtp.gmail.com login torgeir.thoresen@gmail.com port 587 password <pwd>
;;
;; -- gpg howto --
;; https://www.elliotblackburn.com/importing-pgp-keys-from-keybase-into-gpg/
;;
;; gpg --allow-secret-key-import --batch --import keybase-private-key
;; export GPG_TTY=$(tty)
;;
;; still not prompting for a password? try
;; gpgconf --kill all

(require 'epa-file)
(setq epg-gpg-program "gpg2"
      ;; ask only once
      epa-file-cache-passphrase-for-symmetric-encryption t)
(epa-file-enable)

(defun t-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 3))

(evil-define-key 'emacs gnus-group-mode-map
  (kbd "o") 't-gnus-group-list-subscribed-groups)

(evil-define-key 'emacs gnus-group-mode-map
  (kbd "<tab>")
  'gnus-topic-select-group)

(setq smiley-style 'large
      gnus-mime-display-multipart-related-as-mixed t)
(setq gnus-subscribe-groups-done nil)
(defvar gnus-subscribe-groups-done nil
  "Only subscribe groups once.  Or else Gnus will NOT restart.")
(t/add-hook-defun 'gnus-group-mode-hook t-gnus-group-mode-hook
                  (gnus-topic-mode)
                  (unless gnus-subscribe-groups-done
                    (gnus-subscribe-hierarchically "nnimap+gmail:INBOX")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.java.clojure.user")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.java.clojure.pedestal.user")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.lang.javascript.nodejs")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.lang.javascript.clojurescript")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.lang.javascript.v8.general")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.java.springframework.user")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.java.clojure.leiningen")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.internationalization.norwegian")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gwene.com.stackexchange.emacs")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gwene.com.stackoverflow.feeds.org-mode")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gwene.com.youtube.feeds.orgmode.tutorial")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.emacs.help")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.emacs.orgmode")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.line6linux.devel")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.line6linux.user")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.fractal")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.hydrogen.devel")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.hydrogen.user")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.jack.ladish")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.jackit")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.audio.pulseaudio.general")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.hardware.beagleboard.user")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.kde.devel.plasma")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.comp.lang.javascript.v8.devel")

                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.linux.audio.users")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.linux.audio.yoshimi.user")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.linux.ubuntu.announce")
                    (gnus-subscribe-hierarchically "nntp+news.gmane.io:gmane.linux.ubuntu.user.ubuntu-studio")

                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.io.overreacted")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.io.gitlab.porkostomus")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.io.github.practicalli.blog")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.fikesfarm.blog")

                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.org.binchen.blog2")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.theguardian.world.series.eyewitness")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.500px.feed.500px-editors")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.feedburner.peta.pixel")

                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.xkcd")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.org.kk.cooltools")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.lambdaisland.feeds.blog")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.me.tonsky.blog")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.fi.metosin")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.org.emacsen.planet")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.shakthimaan.news")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.feedburner.jlongster")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.ponyfoo.blog.rss.latest")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.wired.news.feeds.rss2")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.arstechnica.arstechnica.baaf")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.feedburner.ommalik")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.no.nrkbeta")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.net.daringfireball.feeds.main")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.smashingmagazi.rss1")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.alistapart.main")

                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.bostonglobe.rss.bigpicture")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.humanwhocodes.feeds.blog")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.addyosmani")
                    (gnus-subscribe-hierarchically "nntp+news.gwene.org:gwene.com.jonathancreame")

                    (comment
                     (setq gnus-topic-topology '(("Gnus" visible nil nil) (("Gmail" visible nil nil)) (("News" visible nil nil)) (("RSS" visible nil nil)) (("Geek" visible nil nil)) (("Linux" invisible nil nil)) (("Langs" visible nil nil)) (("Reddit" invisible nil nil)) (("Hackernews" invisible nil nil)) (("Emacs" visible nil nil)) (("Line6" visible nil nil)) (("Ubuntu Studio" visible nil nil)))))

                    (setq gnus-subscribe-groups-done t)))

;; (evil-collection-gnus-setup)

(evil-define-key 'emacs gnus-summary-mode-map
  ;; motion
  (kbd "p") 'gnus-summary-prev-page
  (kbd "n") 'gnus-summary-next-page
  (kbd "s") (lambda ()
              (interactive)
              (save-excursion
                (call-interactively 'other-window)
                (funcall (t-spray-micro-state
                          (lambda ()
                            (call-interactively 'other-window)))))))

(t/add-hook-defun 'gnus-summary-mode-hook t-gnus-summary-hook)
(t/add-hook-defun 'gnus-article-mode-hook t-gnus-article-hook (darkroom-mode))

;; (t/add-hook-defun 'gnus-exit-gnus-hook t-gnus-exit-hook
;;                   (evil-leader-mode 1))