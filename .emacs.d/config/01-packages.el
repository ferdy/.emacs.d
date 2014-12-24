;;; 01-packages.el --- Part of my Emacs configuration

;;; Commentary:
;; This file stores all the packages related configurations
;; and install/refresh default packages.

;;; Code:
(require 'package)

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
			  smart-mode-line
			  fill-column-indicator
			  page-break-lines
			  ;; General editing
			  hungry-delete
			  adaptive-wrap
			  iedit
			  undo-tree
			  browse-kill-ring
			  smartscan
			  discover
			  discover-my-major
			  expand-region
			  ace-jump-mode
			  easy-kill
			  ;; General coding
			  rainbow-delimiters
			  aggressive-indent
			  ;; Utilities
			  bookmark+
			  names
			  dired+
			  markdown-mode
			  pandoc-mode
			  elfeed
			  paradox
			  sx
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
			  ;; Common Lisp
			  slime
			  slime-company
			  ;; Scheme
			  geiser
			  ;; Company
			  company
			  company-auctex
			  company-math
			  ;; Web development
			  web-mode
			  js2-mode
			  ;; Flycheck
			  flycheck
			  flycheck-pos-tip
			  ;; Search and replace
			  ag
			  wgrep wgrep-ag
			  visual-regexp
			  ;; Project management
			  projectile)
  "Default packages.")

(defun custom/packages-installed-p ()
  "Check if all required packages are already installed."
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
