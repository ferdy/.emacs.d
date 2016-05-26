;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; URL: https://github.com/manuel-uberti/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; This is the GNU Emacs configuration of Manuel Uberti.

;;; Code:

;;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      `(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setq message-log-max 10000)            ; Debugging

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 50000000)

;; Reset threshold to its default after Emacs has startup, because a large
;; GC threshold equates to longer delays whenever GC happens
(defun mu-set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

(add-hook 'emacs-startup-hook 'mu-set-gc-threshold)

;; Bootstrap `use-package' and `dash'
(unless (and (package-installed-p 'use-package)
             (package-installed-p 'dash))
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'dash))

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'subr-x)
(require 'time-date)

;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(setq inhibit-default-init t)           ; Disable the site default settings

;; Warn if the current build is more than a week old
(run-with-idle-timer
 2 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))

;;; Environment fixup
(use-package exec-path-from-shell
  :ensure t
  :if (display-graphic-p)
  :config
  (when (string-match-p "/zsh$" (getenv "SHELL"))
    ;; Use a non-interactive login shell.  A login shell, because my
    ;; environment variables are mostly set in `.zprofile'.
    (setq exec-path-from-shell-arguments '("-l")))

  (dolist (var '("EMAIL" "INFOPATH" "JAVA_OPTS"))
    (add-to-list 'exec-path-from-shell-variables var))

  (exec-path-from-shell-initialize)

  ;; Re-initialize the `Info-directory-list' from $INFOPATH.  Since package.el
  ;; already initializes info, we need to explicitly add the $INFOPATH
  ;; directories to `Info-directory-list'.  We reverse the list of info paths
  ;; to prepend them in proper order subsequently
  (with-eval-after-load 'info
    (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
      (when dir
        (add-to-list 'Info-directory-list dir)))))

;; Set separate custom file for the customize interface
(defconst mu-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file mu-custom-file
        custom-buffer-done-kill nil    ; Kill when existing
        custom-buffer-verbose-help nil ; Remove redundant help text
        ;; Show me the real variable name
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  :init (load mu-custom-file 'no-error 'no-message))

;; Set the directory where all backup and autosave files will be saved
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      backup-by-copying      t          ; Prefer copy to renaming
      version-control        t          ; Version number for backup files
      kept-old-versions      0          ; Do not keep old versions
      delete-old-versions    t)

(setq auto-save-list-file-prefix     "~/.emacs.d/autosave/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; Personal informations
(setq user-full-name "Manuel Uberti")
(setq user-mail-address "manuel@boccaperta.com")

;; The server of `emacsclient'
(use-package server
  :defer t
  :config
  ;; Start server only if it is not already running
  (unless (server-running-p)
    (server-mode)))

(setq confirm-kill-emacs #'y-or-n-p)    ; Confirm before quitting Emacs

;;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package mu-style)
(use-package mu-keybindings)
(use-package mu-pairs)
(use-package mu-highlight)
(use-package mu-buffers)
(use-package mu-windows)
(use-package mu-ivy)
(use-package mu-editing)
(use-package mu-navigation)
(use-package mu-search)
(use-package mu-files :defer 2)
(use-package mu-dired :defer 2)
(use-package mu-completion :defer 3)
(use-package mu-languages)
(use-package mu-latex :defer 2)
(use-package mu-project)
(use-package mu-vers-control :defer 3)
(use-package mu-net)
(use-package mu-utilities)
(use-package mu-org :defer 1)
(use-package mu-flycheck :defer 1)
(use-package mu-programming)
(use-package mu-shells)
(use-package mu-functions)

;;; init.el ends here
