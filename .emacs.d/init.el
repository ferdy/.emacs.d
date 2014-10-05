;;;; Emacs init file

;;; This file sets username and mail address
;;; and loads the different configuration files
;;; I have in ~/.emacs.d/config.

;; Set user and mail address
(setq user-full-name "Manuel Uberti")
(setq user-mail-address "manuel@boccaperta.com")

;; Load all ".el" files under ~/.emacs.d/config directory.
(load "~/.emacs.d/load-directory")
(load-directory "~/.emacs.d/config")

;; Set separate custom file for the customize interface
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
