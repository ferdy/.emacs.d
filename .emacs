;; Emacs init file
;;
;; This file simply loads the different configuration files
;; I have in ~/.emacs.d/elisp.
;;
;; This setup makes the maintenance easier.

;; this is where all the configurations file are
(add-to-list 'load-path "~/.emacs.d/config/")

;; load packages configuration
(load-library "packages")

;; load style configuration
(load-library "style")

;; load mode configuration
(load-library "modes")

;; load keybindings configuration
(load-library "keybindings")
