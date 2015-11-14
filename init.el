;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

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

;; This file sets up packages, custom file, username and mail address. It also
;; loads the different configuration files I have in ~/.emacs.d/lisp.

;;; Code:

;;; Package setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq load-prefer-newer t)         ; Always load newer compiled files
(setq message-log-max 10000)       ; Debugging

;; Allow more than 800Kb cache during init
(setq gc-cons-threshold 50000000)

;; Reset threshold to its default after Emacs has startup, because a large
;; GC threshold equates to longer delays whenever GC happens
(defun mu-set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))

(add-hook 'emacs-startup-hook 'mu-set-gc-threshold)

;; Verify secure connections
(setq gnutls-verify-error t)
(unless (gnutls-available-p)
  (run-with-idle-timer
   2 nil
   (lambda ()
     (lwarn 'emacs
            :warning
            "GNUTLS is missing!  Certificate validation _not_ configured"))))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'diminish)
(require 'bind-key)
(require 'grep)
(require 'dash)
(require 'rx)
(require 'time-date)

;;; Initialization
(when (version< emacs-version "25")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(setq inhibit-default-init t) ; Disable the site default settings

;; Warn if the current build is more than a week old
(run-with-idle-timer
 2 nil
 (lambda ()
   (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
     (when (> (time-to-number-of-days time-since-build) 7)
       (lwarn 'emacs :warning "Your Emacs build is more than a week old!")))))

;; Set separate custom file for the customize interface
(defconst mu-custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config (setq custom-file mu-custom-file
                custom-buffer-done-kill nil    ; Kill when existing
                custom-buffer-verbose-help nil ; Remove redundant help text
                ;; Show me the real variable name
                custom-unlispify-tag-names nil
                custom-unlispify-menu-entries nil)
  :init (load mu-custom-file 'no-error 'no-message))

;; Set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; Personal informations
(setq user-full-name "Manuel Uberti")
(setq user-mail-address "manuel@boccaperta.com")

;; The server of `emacsclient'
(use-package server
  :defer t
  ;; Start server only if it is not already running
  :config (unless (server-running-p)
            (server-mode))
  :diminish (server-buffer-clients . " ⓒ"))

;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package mu-style)
(use-package mu-pairs)
(use-package mu-keybindings)
(use-package mu-highlight)
(use-package mu-buffers)
(use-package mu-windows)
(use-package mu-helm)
(use-package mu-editing)
(use-package mu-navigation)
(use-package mu-search)
(use-package mu-files :defer 2)
(use-package mu-completion :defer 3)
(use-package mu-languages)
(use-package mu-latex)
(use-package mu-vers-control :defer 5)
(use-package mu-net)
(use-package mu-utilities)
(use-package mu-org :defer 1)
(use-package mu-programming)
(use-package mu-project)
(use-package mu-shells)
(use-package mu-bugs)
(use-package mu-media)
(use-package mu-functions)

;;; init.el ends here
