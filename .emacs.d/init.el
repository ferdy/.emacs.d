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
;; This file sets username and mail address and loads the different configuration
;; files I have in ~/.emacs.d/config.

;;; Code:

;;; Initialization
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
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Require files under ~/.emacs.d/config
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require '01-packages.el)
(require '02-functions.el)
(require '03-style.el)
(require '04-editing.el)
(require '05-modes.el)
(require '06-keybindings.el)

;;; init.el ends here
