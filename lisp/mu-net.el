;;; mu-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for network and web utilities.

;;; Code:

(use-package tramp                      ; Remote editing
  :config
  ;; Without this change, tramp ends up sending hundreds of shell commands to
  ;; the remote side to ask what the temporary directory is.
  (put 'temporary-file-directory 'standard-value '("/tmp"))

  (validate-setq
   tramp-verbose 1                      ; Reduce verbosity
   tramp-default-method "ssh"
   tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
   auto-save-file-name-transforms nil)

  (add-to-list 'backup-directory-alist (cons tramp-file-name-regexp nil)))

(use-package circe                      ; Light IRC client
  :ensure t
  :bind ("C-c a c" . circe)
  :config
  ;; Load and set my credentials.  Requires in ~/.ercpass the format
  ;; (:my-pass "password")
  (defvar mu-credentials-file "~/.ercpass")

  (defun mu-retrieve-irc-password (_)
    (with-temp-buffer
      (insert-file-contents-literally mu-credentials-file)
      (plist-get (read (buffer-string)) :my-pass)))

  (validate-setq circe-network-options
                 '(("Freenode"
                    :tls t
                    :pass mu-retrieve-irc-password
                    :channels ("#emacs" "#haskell"))))

  (validate-setq
   circe-default-nick "manuel-uberti"
   circe-default-user "manuel-uberti"
   circe-default-realname "Manuel Uberti"
   circe-default-part-message "Bye!"
   circe-default-quit-message "Bye!"
   circe-use-cycle-completion t
   circe-reduce-lurker-spam t
   circe-format-self-say "<{nick}> {body}"
   circe-format-server-topic "*** Topic Change by {userhost}: {topic-diff}"
   circe-server-buffer-name "{network}"
   circe-prompt-string (propertize ">> " 'face 'circe-prompt-face)))

(use-package circe-color-nicks          ; Colour nicknames
  :after circe
  :config (validate-setq circe-color-nicks-everywhere t))

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
    (kill-buffer "*elfeed-log*")
    (mu-pop-window-configuration))

  ;; Use a single full frame for elfeed
  (with-eval-after-load 'elfeed
    (fullframe elfeed mu-elfeed-quit))

  (validate-setq
   elfeed-use-curl t                    ; Use curl to fetch the feeds
   elfeed-search-title-max-width 80)

  (elfeed-set-timeout 30)               ; Increase timeout

  (validate-setq
   elfeed-feeds
   '(("https://asiaincinema.com/feed/" cinema)
     ("https://asianfilmstrike.wordpress.com/feed/" cinema)
     ("https://cavallette.noblogs.org/feed" security)
     ("http://chinafilminsider.com/feed/" cinema)
     ("https://cinebeats.wordpress.com/feed/" cinema)
     ("http://feed.dilbert.com/dilbert/daily_strip" comic)
     ("https://feeds.feedburner.com/birthmoviesdeath" cinema)
     ("https://feeds2.feedburner.com/PatrickRothfuss" books)
     ("https://film.avclub.com/rss" cinema)
     ("https://filmstudiesforfree.blogspot.com/feeds/posts/default" cinema)
     ("https://haskellweekly.news/haskell-weekly.atom" haskell)
     ("https://news.ycombinator.com/rss" news)
     ("http://outlawvern.com/feed/" cinema)
     ("https://planet.haskell.org/atom.xml" haskell)
     ("http://planet.emacsen.org/atom.xml" emacs)
     ("http://reverseshot.org/archive/entry/rss" cinema)
     ("http://sensesofcinema.com/feed/" cinema)
     ("http://www.commitstrip.com/en/feed/" comic)
     ("http://www.easternkicks.com/feed/" cinema)
     ("https://www.hongkongfp.com/feed" news)
     ("http://www.lastampa.it/italia/politica/rss.xml" news)
     ("http://www.princerecordings.com/feed/" music)
     ("http://www.rogerebert.com/feed/" cinema)
     ("http://www.savagechickens.com/feed" comic)
     ("http://www.signature-reads.com/feed/" books)
     ("http://www.slantmagazine.com/rss" cinema)
     ("https://www.theguardian.com/uk/environment/rss" news)
     ("https://www.theguardian.com/uk-news/rss" news)
     ("https://www.theguardian.com/world/rss" news)
     ("http://www.vcinemashow.com/feed/" cinema))))

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

(use-package paradox                    ; Better package manager interface
  :ensure t
  :bind (("C-c a p" . mu-paradox-open)
         ("C-c a P" . paradox-upgrade-packages)
         :map paradox-menu-mode-map
         ("q" . mu-pop-window-configuration))
  :config
  (defun mu-paradox-open ()
    "Save window configuration and call `paradox-list-packages'."
    (interactive)
    (mu-save-wins-then-call 'paradox-list-packages "nil"))

  ;; Use a single full frame for Paradox
  (with-eval-after-load 'paradox
    (fullframe paradox-list-packages mu-pop-window-configuration))

  (validate-setq
   paradox-github-token t             ; Don't ask for a token
   paradox-execute-asynchronously nil ; No async updates
   paradox-use-homepage-buttons nil   ; Hide download button
   paradox-automatically-star nil     ; Don't star packages automatically
   paradox-display-download-count t   ; Show all possible counts
   paradox-display-star-count t)

  ;; Don't need paradox report
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-print)
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-display-if-noquery))

(use-package browse-url                 ; Browse URLs
  :config
  (validate-setq
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program "opera"))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode))
  :hook ((prog-mode . goto-address-prog-mode)
         (text-mode . goto-address-mode)
         (org-mode  . (lambda () (goto-address-mode -1)))))

(use-package eww                        ; Built-in web browser
  :bind  (("C-c a w b" . eww-list-bookmarks)
          ("C-c a w w" . eww)
          ("C-c a w u" . eww-browse-url))
  :config
  (defun mu-eww-keep-lines (regexp)
    "Show only the lines matching regexp in the web page.
Call `eww-reload' to undo the filtering."
    (interactive (list (read-from-minibuffer
                        "Keep only lines matching regexp: ")))
    (save-excursion
      (read-only-mode -1)
      (goto-char (point-min))
      (keep-lines regexp)
      (read-only-mode 1))))

;;; Utilities and key bindings
;; Toggle image display on/off, especially useful in eww
(defvar-local mu-display-images t)

;;;###autoload
(defun mu-toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (validate-setq mu-display-images (null mu-display-images))
  (mu-backup-display-property mu-display-images))

(defun mu-backup-display-property (invert &optional object)
  "Move the 'display property at POS to 'display-backup.
Only applies if display property is an image.
If INVERT is non-nil, move from 'display-backup to 'display
instead.
Optional OBJECT specifies the string or buffer.  Nil means current
buffer."
  (let* ((inhibit-read-only t)
         (from (if invert 'display-backup 'display))
         (to (if invert 'display 'display-backup))
         (pos (point-min))
         left prop)
    (while (and pos (/= pos (point-max)))
      (if (get-text-property pos from object)
          (setq left pos)
        (setq left (next-single-property-change pos from object)))
      (if (or (null left) (= left (point-max)))
          (setq pos nil)
        (setq prop (get-text-property left from object))
        (setq pos (or (next-single-property-change left from object)
                      (point-max)))
        (when (eq (car prop) 'image)
          (add-text-properties left pos (list from nil to prop) object))))))

(provide 'mu-net)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-net.el ends here
