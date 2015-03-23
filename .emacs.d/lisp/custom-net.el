;;; custom-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for network and web utilities.

;;; Code:

;;; Remote editing
(use-package tramp
  :config
  (progn
    (setq tramp-default-method "ssh"
          tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
          auto-save-file-name-transforms nil)
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))))

;;; Web
;; Requires in ~/.ercpass the format
;; (setq variable "nickname")
;; (setq variable "password")
(use-package erc ; IRC client
  :defer t
  :config
  (progn
    (load "~/.ercpass")
    (require 'erc-services)
    (erc-services-mode 1)

    (setq erc-nick gp-nick
          erc-prompt-for-nickserv-password nil
          erc-nickserve-passwords `((freenode (,gp-nick . ,gp-pass))))

    ;; Disable hl-line-mode in erc
    (add-hook 'erc-mode-hook (lambda ()
                               (setq-local global-hl-line-mode
                                           nil)))))
(use-package elfeed ; RSS feed reader
  :ensure t
  :defer t
  :bind (("<f5>" . elfeed))
  :config
  (progn
    (setq elfeed-feeds
          '(("http://planet.emacsen.org/atom.xml" emacs)
            ("http://planet.clojure.in/atom.xml" clojure)
            ("http://feeds.feedburner.com/disclojure?format=xml" clojure)
            ("http://flashstrap.blogspot.com/feeds/posts/default" music)
            ("http://jazzfromitaly.blogspot.it/feeds/posts/default" music)
            ("http://www.wumingfoundation.com/giap/?feed=rss2" book)
            ("https://cavallette.noblogs.org/feed" security)))

    ;; Elfeed: mark all feed as read
    (require 'elfeed-search)

    (defun elfeed-mark-all-as-read ()
      "Mark all fees as read."
      (interactive)
      (call-interactively 'mark-whole-buffer)
      (elfeed-search-untag-all-unread))

    (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)))

(use-package sx ; StackExchange client for Emacs
  :ensure t
  :defer t
  :bind (("C-c w s" . sx-tab-frontpage)
         ("C-c w S" . sx-tab-newest)
         ("C-c w a" . sx-ask)))

(use-package sx-compose
  :ensure sx
  :defer t
  :config
  (progn
    ;; Don't fill in SX questions/answers, and use visual lines instead. Plays
    ;; more nicely with the website.
    (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
    (add-hook 'sx-compose-mode-hook #'visual-line-mode)))

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package paradox ; Better package manager interface
  :ensure t
  :defer t
  :bind (("<f4>" . paradox-list-packages)
         ("S-<f4>" . paradox-upgrade-packages))
  :config
  (setq paradox-github-token t ; Don't ask for a token, please
        ;; No async for now
        paradox-execute-asynchronously nil)

  ;; Don't need paradox report
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-display-if-noquery))

(provide 'custom-net)

;;; custom-net.el ends here
