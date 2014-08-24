;; Emacs init file
;;
;; This file simply loads the different configuration files
;; I have in ~/.emacs.d/elisp.
;;
;; This setup makes the maintenance easier.

;;; Load all ".el" files under ~/.emacs.d/config directory.
(load "~/.emacs.d/load-directory")
(load-directory "~/.emacs.d/config")
