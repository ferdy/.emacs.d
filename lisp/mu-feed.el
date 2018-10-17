;;; mu-feed.el --- Part of my Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration of elfeed.

;;; Code:

(use-package elfeed                     ; RSS feed reader
  :ensure t
  :bind (("C-c a f" . mu-elfeed-open)
         :map elfeed-search-mode-map
         ("q" . mu-elfeed-quit))
  :config
  (defun mu-elfeed-open ()
    "Save window configuration and call `elfeed'."
    (interactive)
    (mu-save-wins-then-call 'elfeed))

  (defun mu-elfeed-quit ()
    "Save feeds database, kill log buffer and restore window configuration."
    (interactive)
    (elfeed-db-save)
    (when (get-buffer "*elfeed-log*")
      (kill-buffer "*elfeed-log*"))
    (mu-pop-window-configuration))

  ;; Use a single full frame for elfeed
  (with-eval-after-load 'elfeed
    (fullframe elfeed mu-elfeed-quit))

  (setq elfeed-use-curl t
        elfeed-search-title-max-width 80)

  (elfeed-set-timeout 30)               ; Increase timeout

  (setq
   elfeed-feeds
   '(("https://asiaincinema.com/feed/" cinema)
     ("https://asianfilmstrike.wordpress.com/feed/" cinema)
     ("https://cavallette.noblogs.org/feed" security)
     ("http://chinafilminsider.com/feed/" cinema)
     ("https://cinebeats.wordpress.com/feed/" cinema)
     ("https://cinephiliabeyond.org/feed/rss/" cinema)
     ("https://curtsiesandhandgrenades.blogspot.com/feeds/posts/default" cinema)
     ("https://feeds.feedburner.com/birthmoviesdeath" cinema)
     ("https://film.avclub.com/rss" cinema)
     ("https://filmstudiesforfree.blogspot.com/feeds/posts/default" cinema)
     ("https://girishshambu.blogspot.com/feeds/posts/default" cinema)
     ("https://haskellweekly.news/haskell-weekly.atom" haskell)
     ("https://listen2prince.blogspot.com/feeds/posts/default" music)
     ("https://lithub.com/feed/" books)
     ("http://lwlies.com/feed/" cinema)
     ("https://news.ycombinator.com/rss" news)
     ("http://outlawvern.com/feed/" cinema)
     ("http://planet.clojure.in/atom.xml" clojure)
     ("https://planet.haskell.org/atom.xml" haskell)
     ("http://planet.emacsen.org/atom.xml" emacs)
     ("http://reverseshot.org/archive/entry/rss" cinema)
     ("https://selfstyledsiren.blogspot.com/feeds/posts/default" cinema)
     ("http://sensesofcinema.com/feed/" cinema)
     ("https://sergioleoneifr.blogspot.com/feeds/posts/default" cinema)
     ("http://www.500princesongs.com/feed" music)
     ("http://www.aintitcool.com/node/feed/" cinema)
     ("http://www.anothergaze.com/feed/" cinema)
     ("http://www.commitstrip.com/en/feed/" comic)
     ("http://www.easternkicks.com/feed/" cinema)
     ("http://www.haskell-ita.it/atom.xml" haskell)
     ("https://www.hongkongfp.com/feed" news)
     ("https://www.indiewire.com/feed/" cinema)
     ("http://www.lastampa.it/italia/politica/rss.xml" news)
     ("http://www.princerecordings.com/feed/" music)
     ("http://www.rogerebert.com/feed/" cinema)
     ("http://www.savagechickens.com/feed" comic)
     ("http://www.slantmagazine.com/rss" cinema)
     ("http://www.thecinephiliacs.net/feeds/posts/default" cinema)
     ("https://www.theguardian.com/uk/environment/rss" news)
     ("https://www.theguardian.com/world/rss" news)
     ("http://www.vcinemashow.com/feed/" cinema)
     ("https://www.wumingfoundation.com/giap/feed/" books))))

(use-package elfeed-search              ; List feed entries
  :ensure elfeed
  :after elfeed
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread")

  (defun mu-elfeed-mark-all-read ()
    "Mark all feeds as read."
    (interactive)
    (call-interactively 'mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (bind-key "R" #'mu-elfeed-mark-all-read elfeed-search-mode-map))

(provide 'mu-feed)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-feed.el ends here
