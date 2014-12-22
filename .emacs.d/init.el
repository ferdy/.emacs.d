;;; init.el --- Emacs configuration of Manuel Uberti -*- lexical-binding: t; -*-
;;
;; Author: Manuel Uberti <manuel@boccaperta.com>
;; URL: https://gihub.com/boccaperta-it/emacs
;; Keywords: setup

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
(when (version< emacs-version "25")
  (error "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; Set user and mail address
(setq user-full-name "Manuel Uberti")
(setq user-mail-address "manuel@boccaperta.com")

;; Set separate custom file for the customize interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Load all ".el" files under ~/.emacs.d/config directory.
(load "~/.emacs.d/load-directory")
(load-directory "~/.emacs.d/config")

;; An Emacs server for `emacsclient'
(require 'server)
(unless (server-running-p) (server-start))

;;; init.el ends here
