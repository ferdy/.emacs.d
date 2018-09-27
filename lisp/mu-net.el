;;; mu-net.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

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
   paradox-spinner-type 'triangle       ; Prettier spinner
   paradox-github-token t               ; Don't ask for a token
   paradox-execute-asynchronously nil
   paradox-use-homepage-buttons nil
   paradox-automatically-star nil
   paradox-display-star-count nil)

  ;; Don't need paradox report
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-print)
  (remove-hook 'paradox-after-execute-functions
               #'paradox--report-buffer-display-if-noquery))

(use-package browse-url                 ; Browse URLs
  :config
  (validate-setq
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program "google-chrome"))

(use-package goto-addr                  ; Make links clickable
  :defer t
  :bind (:map goto-address-highlight-keymap
              ("<RET>"   . goto-address-at-point)
              ("M-<RET>" . newline))
  :hook ((eshell-mode . goto-address-mode)
         (org-mode    . (lambda () (goto-address-mode -1)))
         (prog-mode   . goto-address-prog-mode)
         (shell-mode  . goto-address-mode)
         (text-mode   . goto-address-mode)))

(use-package eww                        ; Built-in web browser
  :defer t
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
Only applies if display property is an image. If INVERT is
non-nil, move from 'display-backup to 'display instead. Optional
OBJECT specifies the string or buffer. Nil means current buffer."
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

(defun curl (url)
  "Put the content from URL in the current buffer."
  (interactive "sEnter URL: ")
  (url-insert-file-contents url nil nil nil t))

(provide 'mu-net)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-net.el ends here
