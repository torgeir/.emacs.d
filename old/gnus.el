(when (not (getenv "USER_FULLNAME")) (setenv "USER_FULLNAME" "Torgeir Thoresen"))
(when (not (getenv "USER")) (message "Need env USER"))

(setq t-dotted-full-name (replace-regexp-in-string " " "." (getenv "USER_FULLNAME"))
      user-full-name (getenv "USER_FULLNAME")
      user-mail-address (downcase (concat t-dotted-full-name "@gmail.com")))

(darkroom-mode -1)
(darkroom-tentative-mode -1)

(setq gnus-activate-level 3
      gnus-level-subscribed 5
      doom-modeline-gnus-timer 45
      gnus-use-full-window nil ; don't hijack window configuration
      gnus-use-cache t
      gnus-select-method '(nnnil "")
      nnimap-split-fancy nnmail-split-fancy
      nnmail-split-fancy '(| ("Subject" ".*github.*" "github")
                             "misc.misc")
      ;; Reply to mails with matching email address
      gnus-posting-styles '((".*"
                             (address (concat user-full-name " <" t-dotted-full-name "@gmail.com>"))
                             (signature "T")))
      gnus-secondary-select-methods
      '((nntp "news.gmane.io")
        (nntp "news.gwene.org")
        ))


;; search like in gmail in nnimap through nnir
;;(add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))

(t/add-hook-defun 'message-mode-hook t-msg-mode-hook (whitespace-mode -1))

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
;; https://unix.stackexchange.com/questions/184947/how-to-import-secret-gpg-key-copied-from-one-machine-to-another
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

(setq t-gnus-groups-rss
      '("nntp+news.gwene.org:gwene.io.overreacted"
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
        "nntp+news.gwene.org:gwene.com.jonathancreame"
        "nntp+news.gwene.org:gwene.com.echojs"))

(setq t-gnus-groups-news
      '("nntp+news.gmane.io:gmane.comp.java.clojure.user"
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

(t/after doom-modeline
  (setq doom-modeline-gnus-excluded-groups
        (-concat
         t-gnus-groups-rss
         t-gnus-groups-news
         '("nnfolder+archive:sent.2020-10"
           "nnfolder+archive:sent.2021-01"
           "nnfolder+archive:sent.2021-04"
           "nnimap+bekk:Deleted Items"
           "nnimap+bekk:Junk Email"
           "nnimap+gmail:[Gmail]/All Mail"
           "nnimap+gmail:[Gmail]/Important"
           "nnimap+gmail:[Gmail]/Spam"
           ))))

(setq gnus-subscribe-groups-done nil)
(defvar gnus-subscribe-groups-done nil
  "Only subscribe groups once.  Or else Gnus will NOT restart.")
(t/add-hook-defun 'gnus-group-mode-hook t-gnus-group-mode-hook
                  (gnus-topic-mode)

                  (setq gnus-parameters
                        '(("nnimap.*"
                                        ;(gnus-use-scoring nil)
                           (expiry-wait . 2)
                           (display . all))))

                  (setq gnus-topic-topology
                        '(("Gnus" visible nil nil)
                          (("News" visible nil nil))
                          (("Rss" visible nil nil))))

                  (unless gnus-subscribe-groups-done
                    (let ((news t-gnus-groups-news)
                          (rss t-gnus-groups-rss))
                      (setq gnus-topic-alist
                            `(("News" ,@news)
                              ("Rss" ,@rss)))
                      (dolist (sub `(,@news ,@rss))
                        (gnus-subscribe-hierarchically sub)))

                    (setq gnus-subscribe-groups-done t)))

;; (evil-collection-gnus-setup)
(evil-define-key 'emacs gnus-summary-mode-map
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