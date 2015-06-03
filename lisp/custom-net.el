;;; custom-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for network and web utilities.

;;; Code:

;;; Remote editing
(use-package tramp
  :config (progn
            (setq tramp-default-method "ssh"
                  tramp-shell-prompt-pattern
                  "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
                  auto-save-file-name-transforms nil)
            (add-to-list 'backup-directory-alist
                         (cons tramp-file-name-regexp nil))))

;;; Web
;; Requires in ~/.ercpass the format
;; (setq variable "nickname")
(use-package erc ; IRC client
  :bind ("C-c w e" . erc)
  :config (progn
            (load "~/.ercpass")
            (use-package erc-services
              :defer t
              :config (erc-services-mode 1))

            (setq erc-nick gp-nick) ; Set nickname

            ;; Disable hl-line-mode in erc
            (add-hook 'erc-mode-hook (lambda ()
                                       (setq-local global-hl-line-mode
                                                   nil)))))
(use-package elfeed ; RSS feed reader
  :ensure t
  :defer t
  :bind ("<f5>" . elfeed)
  :config (progn
            (setq elfeed-feeds
                  '(("http://planet.emacsen.org/atom.xml" emacs)
                    ("http://planet.clojure.in/atom.xml" clojure)
                    ("http://flashstrap.blogspot.com/feeds/posts/default" music)
                    ("http://jazzfromitaly.blogspot.it/feeds/posts/default"
                     music)
                    ("http://www.wumingfoundation.com/giap/?feed=rss2" book)
                    ("https://cavallette.noblogs.org/feed" security)
                    ("http://rt.com/rss/" news)
                    ("http://feeds.feedburner.com/disinfo/oMPh" news)
                    ("https://firstlook.org/theintercept/feed/?rss" news)))

            (setf url-queue-timeout 30) ; Increase url-queue timeout

            ;; Elfeed: mark all feed as read
            (use-package elfeed-search)

            (defun elfeed-mark-all-as-read ()
              "Mark all fees as read."
              (interactive)
              (call-interactively 'mark-whole-buffer)
              (elfeed-search-untag-all-unread))

            (bind-key "R" #'elfeed-mark-all-as-read elfeed-search-mode-map)))

(use-package sx ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c w s" . sx-tab-all-questions)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package sx-compose
  :ensure sx
  :defer t
  :config (progn
            ;; Don't fill in SX questions/answers, and use visual lines instead.
            (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
            (add-hook 'sx-compose-mode-hook #'visual-line-mode)))

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package paradox ; Better package manager interface
  :ensure t
  :bind (("<f4>"   . paradox-list-packages)
         ("S-<f4>" . paradox-upgrade-packages))
  :config (progn
            (setq paradox-github-token t ; Don't ask for a token, please
                  ;; No async for now
                  paradox-execute-asynchronously nil
                  paradox-spinner-type 'rotating-line
                  ;; Hide packages from Emacs Wiki
                  paradox-hide-wiki-packages t)

            ;; Don't need paradox report
            (remove-hook 'paradox-after-execute-functions
                         #'paradox--report-buffer-print)
            (remove-hook 'paradox-after-execute-functions
                         #'paradox--report-buffer-display-if-noquery)))

(use-package browse-url ; Browse URLs
  :bind ("C-c w u" . browse-url)
  :config (setq browse-url-browser-function 'browse-url-generic
                browse-url-generic-program "/home/manuel/firefox/firefox"))

(use-package eww ; Built-in web browser
  :bind (("C-c w b" . eww-list-bookmarks)
         ("C-c w w" . eww))
  :config (progn
            (defun custom/eww-keep-lines (regexp)
              "Show only the lines matching regexp in the web page.
Call `eww-reload' to undo the filtering."
              (interactive (list (read-from-minibuffer
                                  "Keep only lines matching regexp: ")))
              (save-excursion
                (read-only-mode -1)
                (goto-char (point-min))
                (keep-lines regexp)
                (read-only-mode 1)))))

(provide 'custom-net)

;;; custom-net.el ends here
