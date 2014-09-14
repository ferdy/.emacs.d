;;;; Emacs init file

;;; This file loads the different configuration files
;;; I have in ~/.emacs.d/config and starts emacs server
;;; if needed.

;; Load all ".el" files under ~/.emacs.d/config directory.
(load "~/.emacs.d/load-directory")
(load-directory "~/.emacs.d/config")

;; Allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))
