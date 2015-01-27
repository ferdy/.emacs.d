;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; URL: https://gihub.com/boccaperta-it/emacs
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
(require 'package)

;; Add Melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Always load newer compiled files
(setq load-prefer-newer t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Add El-Get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; Initialization
;; See: https://github.com/lunaryorn/.emacs.d
(when (version< emacs-version "25")
  (error "This configuration needs Emacs trunk, but this is %s!" emacs-version))

(defun custom/warn-about-outdated-build ()
  "Warn about outdated build."
  (let ((time-since-build (time-subtract (current-time) emacs-build-time)))
    (when (> (time-to-number-of-days time-since-build) 7)
      (lwarn 'emacs :warning "Your Emacs build is more than a week old!"))))

(run-with-idle-timer 0 nil #'custom/warn-about-outdated-build)

;; Personal informations
(setq user-full-name "Manuel Uberti")
(setq user-mail-address "manuel@boccaperta.com")

;; Set separate custom file for the customize interface
;; See: https://github.com/lunaryorn/.emacs.d
(defconst custom/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(use-package cus-edit
  :defer t
  :config
  (setq custom-file custom/custom-file
	custom-buffer-done-kill nil ; Kill when existing
	custom-buffer-verbose-help nil ; Remove redundant help text
	;; Show me the real variable name
	custom-unlispify-tag-names nil
	custom-unlispify-menu-entries nil)
  :init (load custom/custom-file 'no-error 'no-message))

;; Require files under ~/.emacs.d/lisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require '01-functions)
(require '02-style)
(require '03-editing)
(require '04-modes)
(require '05-keybindings)

;;; init.el ends here
