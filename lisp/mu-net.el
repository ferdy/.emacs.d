;;; mu-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for network and web utilities.

;;; Code:

(use-package tramp                      ; Remote editing
  :bind ("C-c a r c" . tramp-cleanup-all-connections)
  :config
  (validate-setq tramp-default-method "ssh"
                 tramp-shell-prompt-pattern
                 "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
                 auto-save-file-name-transforms nil)
  (add-to-list 'backup-directory-alist
               (cons tramp-file-name-regexp nil)))

;; Requires in ~/.ercpass the format
;; (:my-pass "password")
(use-package circe                      ; Light IRC client
  :ensure t
  :bind ("C-c a w i" . circe)
  :config
  ;; Load and set my credentials
  (setq mu-credentials-file "~/.ercpass")

  (defun mu-retrieve-irc-password (_)
    (with-temp-buffer
      (insert-file-contents-literally mu-credentials-file)
      (plist-get (read (buffer-string)) :my-pass)))

  (validate-setq circe-network-options
                 '(("Freenode"
                    :tls t
                    :pass mu-retrieve-irc-password)))

  (validate-setq
   circe-default-nick "gekkop"
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
   circe-prompt-string (propertize ">> " 'face 'circe-prompt-face)))

(use-package elfeed                     ; RSS feed reader
  :ensure t
  :bind ("C-c a f" . elfeed)
  :config
  (validate-setq
   elfeed-feeds
   '(("http://planet.emacsen.org/atom.xml" emacs)
     ("http://planet.lisp.org/rss20.xml" lisp)
     ("http://planet.clojure.in/atom.xml" lisp)
     ("http://www.scheme.dk/planet/atom.xml" lisp)
     ("https://cavallette.noblogs.org/feed" security)
     ("http://dilbert.oeey.com/" comic)
     ("https://xkcd.com/rss.xml" comic)))

  (validate-setq elfeed-use-curl t)     ; Use curl to fetch the feeds

  ;; Increse timeout
  (elfeed-set-timeout 30))

(use-package elfeed-search              ; List feed entries
  :ensure elfeed
  :after elfeed
  :config
  (defun mu-elfeed-mark-all-read ()
    "Mark all feeds as read."
    (interactive)
    (call-interactively 'mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (bind-key "R" #'mu-elfeed-mark-all-read elfeed-search-mode-map))

(use-package paradox                    ; Better package manager interface
  :ensure t
  :bind (("C-c a p" . paradox-list-packages)
         ("C-c a P" . paradox-upgrade-packages))
  :config
  (validate-setq paradox-github-token t             ; Don't ask for a token
                 paradox-execute-asynchronously nil ; No async updates
                 paradox-spinner-type 'moon         ; Prettier spinner
                 paradox-use-homepage-buttons nil   ; Hide download button
                 paradox-hide-wiki-packages t       ; Hide packages from Emacs Wiki
                 paradox-automatically-star nil     ; Don't star packages automatically
                 ;; Show all possible counts
                 paradox-display-download-count t
                 paradox-display-star-count t)

  ;; Don't need paradox report
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-print)
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-display-if-noquery))

(use-package browse-url                 ; Browse URLs
  :config
  (validate-setq browse-url-browser-function 'browse-url-generic
                 browse-url-generic-program "firefox"))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (("C-c t a" . goto-address-mode)
         ("C-c t A" . goto-address-prog-mode))
  :init
  (add-hook 'prog-mode-hook #'goto-address-prog-mode)
  (add-hook 'text-mode-hook #'goto-address-mode)
  (add-hook 'org-mode-hook (lambda ()
                             (goto-address-mode -1))))

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

(use-package sx                         ; StackExchange client for Emacs
  :ensure t
  :bind (("C-c a S a" . sx-ask)
         ("C-c a S s" . sx-tab-all-questions)
         ("C-c a S q" . sx-tab-all-questions)
         ("C-c a S n" . sx-tab-newest)))

(use-package sx-compose                 ; Write questions/answers for Stack Exchange
  :ensure sx
  :defer t
  :config
  ;; Don't fill in SX questions/answers, and use visual lines instead.  Plays
  ;; more nicely with the website.
  (add-hook 'sx-compose-mode-hook #'turn-off-auto-fill)
  (add-hook 'sx-compose-mode-hook #'visual-line-mode)
  (add-hook 'sx-compose-mode-hook
            #'mu-whitespace-style-no-long-lines)

  ;; Clean up whitespace before sending questions
  (add-hook 'sx-compose-before-send-hook
            (lambda () (whitespace-cleanup) t))

  (bind-key "M-q" #'ignore sx-compose-mode-map))

(use-package sx-question-mode           ; Show Stack
  :ensure sx
  :defer t
  :config
  ;; Display questions in the same window
  (validate-setq sx-question-mode-display-buffer-function #'switch-to-buffer))

;;; Utilities and keybindings
;; Toggle image display on/off, especially useful in eww
(defvar-local mu-display-images t)

;;;###autoload
(defun mu-toggle-image-display ()
  "Toggle images display on current buffer."
  (interactive)
  (validate-setq mu-display-images
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

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-net.el ends here
