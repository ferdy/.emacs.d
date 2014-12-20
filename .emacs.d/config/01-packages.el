;;; 01-packages.el --- Part of my Emacs configuration

;;; Commentary:

;; This file stores all the packages related configurations
;; and install/refresh default packages

;;; Code:
(require 'package)

;; Add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; Add Melpa
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

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
(defvar custom/packages '(;; Theme
			  solarized-theme

			  ;; Utilities
			  hungry-delete
			  adaptive-wrap
			  rainbow-delimiters
			  bookmark+
			  names
			  aggressive-indent
			  iedit
			  undo-tree
			  browse-kill-ring
			  unicode-fonts
			  dired+
			  pandoc-mode
			  elfeed

			  ;; Ido
			  ido-ubiquitous
			  ido-vertical-mode
			  flx-ido
			  smex

			  ;; Org-mode
			  org

			  ;; Magit for version control
			  magit

			  ;; LaTeX
			  auctex
			  ebib
			  latex-extra

			  ;; Clojure
			  clojure-mode
			  cider

			  ;; Company
			  company
			  company-auctex
			  company-math

			  ;; Web development
			  web-mode
			  js2-mode

			  ;; Flycheck
			  flycheck
			  flycheck-pos-tip)

  "Default packages.")

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

;;; 01-packages.el ends here
