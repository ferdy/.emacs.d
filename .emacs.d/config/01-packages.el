;;;; 01-packages.el

;;; This file stores all the packages related configurations
;;; and install/refresh default packages

(require 'package)
(package-initialize)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; Add Melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Use El-Get to sync repos and dependencies.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; Default packages
;; See: http://www.aaronbedra.com/emacs.d/
(defvar custom/packages '(solarized-theme
			  ido-ubiquitous
			  ido-vertical-mode
			  flx-ido
			  smex
			  hungry-delete
			  org
			  magit
			  auctex
			  ebib
			  latex-extra
			  adaptive-wrap
			  clojure-mode
			  cider
			  pandoc-mode
			  rainbow-delimiters
			  bookmark+
			  names
			  aggressive-indent
			  elfeed
			  dired+
			  company
			  company-auctex
			  undo-tree)
  "Default packages")

(defun custom/packages-installed-p ()
  (loop for pkg in custom/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (custom/packages-installed-p)
  (message "%s" "Refreshing packages database...")
  (package-refresh-contents)
  (dolist (pkg custom/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Always load newer compiled files
(setq load-prefer-newer t)
