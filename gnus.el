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

                  (setq gnus-topic-topology
                        '(("Gnus" visible nil nil)
                          (("Gmail" visible nil nil))
                          (("News" visible nil nil))
                          (("Rss" visible nil nil))))

                  (unless gnus-subscribe-groups-done

                    (let ((mail '("nnimap+gmail:INBOX"
                                  "nnimap+gmail:saga"
                                  "nnimap+gmail:fun"
                                  "nnimap+gmail:github"))
                          (news '("nntp+news.gmane.io:gmane.comp.java.clojure.user"
                                  "nntp+news.gmane.io:gmane.comp.java.clojure.pedestal.user"
                                  "nntp+news.gmane.io:gmane.comp.lang.javascript.nodejs"
                                  "nntp+news.gmane.io:gmane.comp.lang.javascript.clojurescript"
                                  "nntp+news.gmane.io:gmane.comp.lang.javascript.v8.general"
                                  "nntp+news.gmane.io:gmane.comp.java.springframework.user"
                                  "nntp+news.gmane.io:gmane.comp.java.clojure.leiningen"
                                  "nntp+news.gmane.io:gmane.comp.internationalization.norwegian"
                                  "nntp+news.gmane.io:gwene.com.stackexchange.emacs"
                                  "nntp+news.gmane.io:gwene.com.stackoverflow.feeds.org-mode"
                                  "nntp+news.gmane.io:gwene.com.youtube.feeds.orgmode.tutorial"
                                  "nntp+news.gmane.io:gmane.emacs.help"
                                  "nntp+news.gmane.io:gmane.emacs.orgmode"
                                  "nntp+news.gmane.io:gmane.comp.audio.line6linux.devel"
                                  "nntp+news.gmane.io:gmane.comp.audio.line6linux.user"
                                  "nntp+news.gmane.io:gmane.comp.audio.fractal"
                                  "nntp+news.gmane.io:gmane.comp.audio.hydrogen.devel"
                                  "nntp+news.gmane.io:gmane.comp.audio.hydrogen.user"
                                  "nntp+news.gmane.io:gmane.comp.audio.jack.ladish"
                                  "nntp+news.gmane.io:gmane.comp.audio.jackit"
                                  "nntp+news.gmane.io:gmane.comp.audio.pulseaudio.general"
                                  "nntp+news.gmane.io:gmane.comp.hardware.beagleboard.user"
                                  "nntp+news.gmane.io:gmane.comp.kde.devel.plasma"
                                  "nntp+news.gmane.io:gmane.comp.lang.javascript.v8.devel"
                                  "nntp+news.gmane.io:gmane.linux.audio.users"
                                  "nntp+news.gmane.io:gmane.linux.audio.yoshimi.user"
                                  "nntp+news.gmane.io:gmane.linux.ubuntu.announce"
                                  "nntp+news.gmane.io:gmane.linux.ubuntu.user.ubuntu-studio"))
                          (rss '("nntp+news.gwene.org:gwene.io.overreacted"
                                 "nntp+news.gwene.org:gwene.io.gitlab.porkostomus"
                                 "nntp+news.gwene.org:gwene.io.github.practicalli.blog"
                                 "nntp+news.gwene.org:gwene.com.fikesfarm.blog"
                                 "nntp+news.gwene.org:gwene.org.binchen.blog2"
                                 "nntp+news.gwene.org:gwene.com.theguardian.world.series.eyewitness"
                                 "nntp+news.gwene.org:gwene.com.500px.feed.500px-editors"
                                 "nntp+news.gwene.org:gwene.com.feedburner.peta.pixel"
                                 "nntp+news.gwene.org:gwene.com.xkcd"
                                 "nntp+news.gwene.org:gwene.org.kk.cooltools"
                                 "nntp+news.gwene.org:gwene.com.lambdaisland.feeds.blog"
                                 "nntp+news.gwene.org:gwene.me.tonsky.blog"
                                 "nntp+news.gwene.org:gwene.fi.metosin"
                                 "nntp+news.gwene.org:gwene.org.emacsen.planet"
                                 "nntp+news.gwene.org:gwene.com.shakthimaan.news"
                                 "nntp+news.gwene.org:gwene.com.feedburner.jlongster"
                                 "nntp+news.gwene.org:gwene.com.ponyfoo.blog.rss.latest"
                                 "nntp+news.gwene.org:gwene.com.wired.news.feeds.rss2"
                                 "nntp+news.gwene.org:gwene.com.arstechnica.arstechnica.baaf"
                                 "nntp+news.gwene.org:gwene.com.feedburner.ommalik"
                                 "nntp+news.gwene.org:gwene.no.nrkbeta"
                                 "nntp+news.gwene.org:gwene.net.daringfireball.feeds.main"
                                 "nntp+news.gwene.org:gwene.com.smashingmagazi.rss1"
                                 "nntp+news.gwene.org:gwene.com.alistapart.main"
                                 "nntp+news.gwene.org:gwene.com.bostonglobe.rss.bigpicture"
                                 "nntp+news.gwene.org:gwene.com.humanwhocodes.feeds.blog"
                                 "nntp+news.gwene.org:gwene.com.addyosmani"
                                 "nntp+news.gwene.org:gwene.com.jonathancreame")))
                      (setq gnus-topic-alist
                            `(("Gmail" ,@mail)
                              ("News" ,@news)
                              ("Rss" ,@rss)))
                      (dolist (sub `(,@mail ,@news ,@rss))
                        (gnus-subscribe-hierarchically sub)))

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
                (forward-paragraph)
                (funcall (t-spray-micro-state
                          (lambda ()
                            (call-interactively 'other-window)))))))

(t/add-hook-defun 'gnus-summary-mode-hook t-gnus-summary-hook)
(t/add-hook-defun 'gnus-article-mode-hook t-gnus-article-hook (darkroom-mode))

;; (t/add-hook-defun 'gnus-exit-gnus-hook t-gnus-exit-hook
;;                   (evil-leader-mode 1))