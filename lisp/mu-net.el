;;; mu-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for network and web utilities.

;;; Code:

(use-package tramp                      ; Remote editing
  :bind ("C-c a r c" . tramp-cleanup-all-connections)
  :config
  (progn
    (setq tramp-default-method "ssh"
          tramp-shell-prompt-pattern
          "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
          auto-save-file-name-transforms nil)
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))))

;; Requires in ~/.ercpass the format
;; (:my-pass "password")
(use-package circe                      ; Light IRC client
  :ensure t
  :bind ("C-c a i" . circe)
  :config
  (progn
    ;; Load and set my credentials
    (setq mu-credentials-file "~/.ercpass")

    (defun mu-retrieve-irc-password (_)
      (with-temp-buffer
        (insert-file-contents-literally mu-credentials-file)
        (plist-get (read (buffer-string)) :my-pass)))

    (setq circe-network-options
          '(("Freenode"
             :tls t
             :pass mu-retrieve-irc-password)))

    (setq circe-default-nick "gekkop"
          circe-default-user "gekkop"
          circe-default-realname "Manuel Uberti"
          circe-default-part-message "Bye!"
          circe-default-quit-message "Bye!"
          circe-use-cycle-completion t
          circe-reduce-lurker-spam t
          circe-format-self-say "<{nick}> {body}"
          circe-format-server-topic
          "*** Topic Change by {userhost}: {topic-diff}"
          circe-server-buffer-name "{network}"
          circe-prompt-string (propertize ">>> " 'face 'circe-prompt-face)
          circe-color-nicks-everywhere t)))

(use-package elfeed                     ; RSS feed reader
  :ensure t
  :bind ("C-c a f" . elfeed)
  :config
  (progn
    (setq elfeed-feeds
          '(("http://planet.emacsen.org/atom.xml" emacs)
            ("http://emacshorrors.com/feed.atom" emacs)
            ("http://emacsninja.com/feed.atom" emacs)
            ("http://planet.lisp.org/rss20.xml" lisp)
            ("http://planet.clojure.in/atom.xml" clojure)
            ("https://cavallette.noblogs.org/feed" security)
            ("https://mjg59.dreamwidth.org/data/rss" security)
            ("http://flashstrap.blogspot.com/feeds/posts/default" music)
            ("http://jazzfromitaly.blogspot.it/feeds/posts/default"
             music)
            ("http://www.avclub.com/feed/rss/?tags=film" film)
            ("http://www.slantmagazine.com/rss?tag=film" film)
            ("http://sensesofcinema.com/feed/" film)
            ("https://mubi.com/notebook/posts.atom" film)
            ("https://cinebeats.wordpress.com/feed/" film)
            ("http://girishshambu.blogspot.it/feeds/posts/default?alt=rss"
             film)
            ("http://filmstudiesforfree.blogspot.it/feeds/posts/default?alt=rss"
             film)
            ("http://quod.lib.umich.edu/f/fc/longfeed.xml" film)))

    ;; Increase url-queue timeout
    (setf url-queue-timeout 30)))

(use-package elfeed-search              ; List feed entries
  :ensure elfeed
  :after elfeed
  :config
  (progn
    ;; Elfeed: mark all feed as read
    (defun elfeed-mark-all-as-read ()
      "Mark all feeds as read."
      (interactive)
      (call-interactively 'mark-whole-buffer)
      (elfeed-search-untag-all-unread))

    (bind-key "R" #'elfeed-mark-all-as-read elfeed-search-mode-map)))

(use-package paradox                    ; Better package manager interface
  :ensure t
  :bind ("C-c a p" . paradox-list-packages)
  :config
  (progn
    (setq paradox-github-token t             ; Don't ask for a token, please
          paradox-execute-asynchronously nil ; No async updates
          paradox-spinner-type 'moon
          paradox-use-homepage-buttons nil   ; Hide download button
          paradox-hide-wiki-packages t       ; Hide packages from Emacs Wiki
          paradox-automatically-star nil
          ;; Show all possible counts
          paradox-display-download-count t
          paradox-display-star-count t)

    ;; Don't need paradox report
    (remove-hook 'paradox-after-execute-functions
                 #'paradox--report-buffer-print)
    (remove-hook 'paradox-after-execute-functions
                 #'paradox--report-buffer-display-if-noquery)))

(use-package browse-url                 ; Browse URLs
  :config (setq browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "/usr/bin/iceweasel"))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode))
  :init
  (progn (add-hook 'prog-mode-hook #'goto-address-prog-mode)
         (add-hook 'text-mode-hook #'goto-address-mode)
         (add-hook 'org-mode-hook (lambda ()
                                    (goto-address-mode -1)))))

(use-package eww                        ; Built-in web browser
  :bind  (("C-c a w b" . eww-list-bookmarks)
          ("C-c a w w" . eww)
          ("C-c a w u" . eww-browse-url))
  :config
  (progn
    (defun mu-eww-keep-lines (regexp)
      "Show only the lines matching regexp in the web page.
Call `eww-reload' to undo the filtering."
      (interactive (list (read-from-minibuffer
                          "Keep only lines matching regexp: ")))
      (save-excursion
        (read-only-mode -1)
        (goto-char (point-min))
        (keep-lines regexp)
        (read-only-mode 1)))))

;;; Utilities and keybindings
;; Toggle image display on/off, especially useful in eww
(defvar-local mu-display-images t)

;;;###autoload
(defun mu-toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq mu-display-images
        (null mu-display-images))
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

;;; mu-net.el ends here
