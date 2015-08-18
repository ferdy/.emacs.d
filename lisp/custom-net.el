;;; custom-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for network and web utilities.

;;; Code:

(use-package tramp ; Remote editing
  :config (progn
            (setq tramp-default-method "ssh"
                  tramp-shell-prompt-pattern
                  "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
                  auto-save-file-name-transforms nil)
            (add-to-list 'backup-directory-alist
                         (cons tramp-file-name-regexp nil))))

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
                    ("http://emacshorrors.com/feed.atom" emacs)
                    ("http://planet.clojure.in/atom.xml" clojure)
                    ("http://flashstrap.blogspot.com/feeds/posts/default" music)
                    ("http://jazzfromitaly.blogspot.it/feeds/posts/default"
                     music)
                    ("http://www.wumingfoundation.com/giap/?feed=rss2" book)
                    ("https://cavallette.noblogs.org/feed" security)))

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

(use-package sx-compose ; Compose questions and answers
  :ensure sx
  :defer t
  :config (progn
            ;; Don't fill in SX questions/answers, and use visual lines instead.
            (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
            (add-hook 'sx-compose-mode-hook #'visual-line-mode)
            (add-hook 'sx-compose-mode-hook
                      #'custom/whitespace-style-no-long-lines)

            ;; Clean up whitespace before sending questions
            (add-hook 'sx-compose-before-send-hook
                      (lambda ()
                        (whitespace-cleanup)
                        t))))

(use-package sx-question-mode ; Display questions
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

(use-package paradox ; Better package manager interface
  :ensure t
  :bind ("<f4>" . paradox-list-packages)
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
                browse-url-generic-program "~/firefox/firefox"))

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

;;; Utilities and keybindings
;; Toggle image display on/off, especially useful in eww
(defvar-local custom/display-images t)

;;;###autoload
(defun custom/toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (setq custom/display-images
        (null custom/display-images))
  (custom/backup-display-property custom/display-images))

(defun custom/backup-display-property (invert &optional object)
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

(provide 'custom-net)

;;; custom-net.el ends here
