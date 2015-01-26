;;; 05-modes.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:
;; This file stores the configurations of every mode I use.

;;; Code:
;; Minor mode to hide the mode line
;; See http://bzg.fr/emacs-hide-mode-line.html
(defvar-local hidden-mode-line-mode nil)
(defvar-local hide-mode-line nil)

(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (force-mode-line-update)
  ;; apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message "Hidden Mode Line Mode enabled.")))

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; DIRED
;; Auto refresh buffers
(global-auto-revert-mode 1)

(use-package dired
  :defer t
  :bind (("C-c z" . dired-get-size))
  :config
  (progn
    ;; Power up dired
    (require 'dired-x)
    ;; Always revert Dired buffers on revisiting
    (setq dired-auto-revert-buffer t
	  dired-listing-switches "-laGh1v --group-directories-first"
	  ;; Also auto refresh dired, but be quiet about it
	  global-auto-revert-non-file-buffers t
	  auto-revert-verbose nil
	  ;; Don't ask about recursive copies
	  dired-recursive-copies 'always)))

(use-package dired-x
  :defer t
  :config
  (progn
    ;; Omit hidden files by default (C-x M-o to show them)
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))))

(use-package dired+
  :ensure t
  :config
  (progn
    ;; Reuse buffer for directories
    (diredp-toggle-find-file-reuse-dir 1)
    (setq diredp-hide-details-initially-flag nil)
    (setq diredp-hide-details-propagate-flag nil)))

(use-package bookmark+
  :ensure t)

;; HARDHAT
(use-package hardhat
  :ensure t
  :defer t
  :idle (global-hardhat-mode))

;; PO-MODE
(use-package po-mode
  :load-path "el-get/po-mode"
  :defer t
  :init
  (progn
    (setq auto-mode-alist
	  (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))))

;; SCHEME
;; Requires: guile-2.0
(use-package geiser
  :ensure t
  :defer t
  :init
  (progn
    (setq scheme-program-name "guile")
    (setq geiser-impl-installed-implementations '(guile))))

;; Use this for Chicken Scheme instead of Guile
;; (setq scheme-program-name "csi -:c"
;; (add-to-list 'load-path "~/githubs/swank-chicken/")
;; (autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;; (setq swank-chicken-path "~/githubs/swank-chicken/swank-chicken.scm")

;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;             (slime-mode t)))

;; SLIME
;; Requires: sbcl, slime, sbcl-doc, cl-clx-sbcl,
;; cl-ppcre, autoconf, texinfo, cl-swank
(use-package slime
  :ensure t
  :defer t
  :init (setq inferior-lisp-program "/usr/bin/sbcl")
  :config (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :defer t
  :init (slime-setup '(slime-company)))

;; ORG-MODE
(use-package org
  :ensure t
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c a" . org-agenda))
  :config
  (progn
    (setq org-src-fontify-natively t
	  org-log-done 'time
	  org-completion-use-ido t
	  org-export-with-smart-quotes t
	  ;; Customize agenda view
	  org-agenda-custom-commands
	  '(("g" "Agenda and giulia-tagged tasks"
	     ((agenda "")
	      (tags-todo "giulia")
	      (tags "giulia")))
	    ("m" "Agenda and manuel-tagged tasks"
	     ((agenda "")
	      (tags-todo "manuel")
	      (tags "manuel"))))
	  ;; Turn off preamble and postamble in HTML export
	  org-html-preamble nil
	  org-html-postamble nil
	  org-export-html-style-default ""
	  org-export-html-style-include-default nil)

    ;; Update parent nodes when child is removed
    (defun myorg-update-parent-cookie ()
      "Update parent nodes when child is removed."
      (when (equal major-mode 'org-mode)
	(save-excursion
	  (ignore-errors
	    (org-back-to-heading)
	    (org-update-parent-todo-statistics)))))

    (defadvice org-kill-line (after fix-cookies activate)
      "Update parent node."
      (myorg-update-parent-cookie))

    (defadvice kill-whole-line (after fix-cookies activate)
      "Update parent node."
      (myorg-update-parent-cookie))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (C .t)
   (awk .t)
   (lisp .t)
   (python .t)
   (scheme .t)))

;; Auto insert custom text upon opening an org file
(auto-insert-mode)
(setq auto-insert-query nil)
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.org\\'" . "Org skeleton")
     '(
       "Short description: "
       "#+STARTUP: showall\n"
       > _ \n \n)))

;; ORG-PRESENT
(use-package org-present
  :disabled t
  :load-path "various"
  :config
  (progn
    (add-hook 'org-present-mode-hook
	      (lambda ()
		(org-present-big)
		(org-display-inline-images t t)))

    (add-hook 'org-present-mode-quit-hook
	      (lambda ()
		(org-present-small)
		(org-remove-inline-images)))))

;; ORG2BLOG
(use-package metaweblog
  :ensure t)

(use-package xml-rpc
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package org2blog
  :ensure t
  :defer t
  :init (require 'org2blog-autoloads)
  :config
  (progn
    (setq org2blog/wp-use-sourcecode-shortcode t
	  org2blog/wp-sourcecode-langs
	  '("bash" "javascript" "php" "text"
	    "xml" "sh" "elisp" "lisp" "lua")
	  org2blog/wp-blog-alist
	  '(("informatica.boccaperta.com"

	     :url "http://informatica.boccaperta.com/xmlrpc.php"
	     :username "manuel")))))

;; DOC-VIEW-MODE
(use-package doc-view
  :defer t
  :config
  (progn
    (setq doc-view-continuous t)))

;; ESHELL
(use-package eshell
  :defer t
  :bind (("<f1>" . eshell-here))
  :config
  (progn
    ;; Clear eshell buffer
    ;; See http://www.khngai.com/emacs/eshell.php
    (defun eshell/clear ()
      "Clear the eshell buffer."
      (interactive)
      (let ((inhibit-read-only t))
	(erase-buffer)))

    (setq eshell-cmpl-cycle-completions nil
	  eshell-save-history-on-exit t)

    ;; Run scrips from current working on remote system
    (defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
      "Run scrips from current working on remote system."
      (setq command (file-truename command)))

    (add-hook 'eshell-mode-hook
	      (lambda ()
		(local-set-key (kbd "C-c h")
			       (lambda ()
				 (interactive)
				 (insert
				  (ido-completing-read "Eshell history: "
						       (delete-dups
							(ring-elements eshell-history-ring))))))
		(local-set-key (kbd "C-c C-h") 'eshell-list-history)))

    ;; Disable smartscan for eshell
    (add-hook 'eshell-mode-hook
	      (lambda ()
		(smartscan-mode -1)))))

;; SHELL
(use-package shell
  :defer t
  :bind (("<f2>" . shell))
  :config
  (progn
    ;; Clear shell buffer
    (defun clear-shell ()
      (interactive)
      (let ((comint-buffer-maximum-size 0))
	(comint-truncate-buffer)))

    (define-key shell-mode-map (kbd "C-l") 'clear-shell)

    ;; Disable smartscan for shell
    (add-hook 'shell-mode-hook
	      (lambda ()
		(smartscan-mode -1)))))

;; DIFF-HL
(use-package diff-hl
  :ensure t
  :defer t
  :init
  (progn
    ;; Highlight changes to the current file in the fringe
    (global-diff-hl-mode)
    ;; Highlight changed files in the fringe of Dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
    ;; Fall back to the display margin, if the fringe is unavailable
    (unless (display-graphic-p)
      (diff-hl-margin-mode))))

;; MAGIT
(use-package magit
  :ensure t
  :defer t
  :bind (("<f3>" . magit-status))
  :config
  (progn
    ;; Shut up, Magit!
    (setq magit-save-some-buffers 'dontask
	  magit-stage-all-confirm nil
	  magit-unstage-all-confirm nil
	  ;; Except when you ask something useful…
	  magit-set-upstream-on-push t
	  ;; Use IDO for completion
	  magit-completing-read-function #'magit-ido-completing-read
	  magit-auto-revert-mode-lighter "")
    ;; Auto-revert files after Magit operations
    (magit-auto-revert-mode)

    ;; Fullscreen magit-status
    ;; See http://whattheemacsd.com/setup-magit.el-01.html
    (defadvice magit-status (around magit-fullscreen activate)
      "Turn fullscreen on for magit-status."
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restore the previous window configuration and kill the magit buffer."
      (interactive)
      (custom/kill-buffers "^\\*magit")
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

;; GIT MODES
(use-package git-commit-mode ; Git commit message mode
  :ensure t
  :defer t)

(use-package gitconfig-mode ; Git configuration mode
  :ensure t
  :defer t)

(use-package gitignore-mode ; .gitignore mode
  :ensure t
  :defer t)

(use-package gitattributes-mode ; Git attributes mode
  :ensure t
  :defer t)

(use-package git-rebase-mode ; Mode for git rebase -i
  :ensure t
  :defer t)

;; AUCTeX
;; Requires: texlive-latex-base, texlive-latex-recommended,
;; latexmk, texlive-latex-extra, texlive-fonts-recommended,
;; texlive-generic-recommended, texlive-xetex
;; texlive-lang-italian, cjk-latex, latex-cjk-all
(use-package tex-site
  :ensure auctex)

;; TeX editing
(use-package tex
  :ensure auctex
  :defer t
  :config
  (progn
    (setq TeX-parse-self t ; Parse documents to provide completion
					; for packages, etc.
	  TeX-auto-save t ; Automatically save style information
	  TeX-electric-sub-and-superscript t ; Automatically insert braces after
					; sub- and superscripts in math mode
	  ;; Don't insert magic quotes right away.
	  TeX-quote-after-quote t
	  ;; Don't ask for confirmation when cleaning
	  TeX-clean-confirm nil
	  ;; Provide forward and inverse search with SyncTeX
	  TeX-source-correlate-mode t
	  TeX-source-correlate-method 'synctex)
    (setq-default TeX-master nil ; Ask for the master file
		  TeX-engine 'luatex ; Use luatex
		  TeX-PDF-mode t)
    ;; Move to chktex
    (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 %s")))

(use-package tex-buf
  :ensure auctex
  :defer t
  ;; Don't ask for confirmation when saving before processing
  :config (setq TeX-save-query nil))

(use-package tex-style
  :ensure auctex
  :defer t
  :config
  ;; Enable support for csquotes
  (setq LaTeX-csquotes-close-quote "}"
	LaTeX-csquotes-open-quote "\\enquote{"))

(use-package tex-fold
  :ensure auctex
  :defer t
  :init (add-hook 'TeX-mode-hook #'TeX-fold-mode))

(use-package tex-mode
  :ensure auctex
  :defer t
  :config
  (font-lock-add-keywords 'latex-mode
			  `((,(rx "\\"
				  symbol-start
				  "fx" (1+ (or (syntax word) (syntax symbol)))
				  symbol-end)
			     . font-lock-warning-face))))

(use-package latex
  :ensure auctex
  :defer t
  :config
  (progn
    ;; No language-specific hyphens please
    (setq LaTeX-babel-hyphen nil
	  LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))) ; Easy math input

(use-package latex-extra
  :ensure t
  :defer t
  :config (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

(use-package auctex-latexmk
  :ensure t
  :defer t
  :init (with-eval-after-load 'latex
	  (auctex-latexmk-setup)))

(use-package bibtex
  :defer t
  :config
  (progn
    ;; Run prog mode hooks for bibtex
    (add-hook 'bibtex-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
    ;; Use a modern BibTeX dialect
    (bibtex-set-dialect 'biblatex)))

;; Configure RefTeX
(use-package reftex
  :defer t
  :init (add-hook 'LaTeX-mode-hook #'reftex-mode)
  :config
  (progn
    ;; Plug into AUCTeX
    (setq reftex-plug-into-AUCTeX t
	  ;; Automatically derive labels, and prompt for confirmation
	  reftex-insert-label-flags '(t t))

    ;; Provide basic RefTeX support for biblatex
    (unless (assq 'biblatex reftex-cite-format-builtin)
      (add-to-list 'reftex-cite-format-builtin
		   '(biblatex "The biblatex package"
			      ((?\C-m . "\\cite[]{%l}")
			       (?t . "\\textcite{%l}")
			       (?a . "\\autocite[]{%l}")
			       (?p . "\\parencite{%l}")
			       (?f . "\\footcite[][]{%l}")
			       (?F . "\\fullcite[]{%l}")
			       (?x . "[]{%l}")
			       (?X . "{%l}"))))
      (setq reftex-cite-format 'biblatex))))

;;; MARKDOWN-MODE
(use-package markdown-mode
  :ensure t
  :config
  (progn
    ;; Use Pandoc to process Markdown
    (setq markdown-command "pandoc -s -f markdown -t html5")))

;; ERC
;; Requires in ~/.ercpass the format
;; (setq variable "nickname")
;; (setq variable "password")
(use-package erc
  :defer t
  :config
  (progn
    (load "~/.ercpass")
    (require 'erc-services)
    (erc-services-mode 1)

    (setq erc-nick gp-nick)
    (setq erc-prompt-for-nickserv-password nil)
    (setq erc-nickserve-passwords
	  `((freenode (,gp-nick . ,gp-pass))))))

;; CLOJURE MODE AND CIDER
;; Requires: openjdk-7-jre, openjdk-7-jre, lein
(use-package cider
  :ensure t
  :defer t
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package clojure-mode
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'clojure-mode-hook #'cider-mode)
    (add-hook 'clojure-mode-hook #'subword-mode)))

;; Extra font-locking for Clojure
(use-package clojure-mode-extra-font-locking
  :ensure clojure-mode
  :defer t
  :init (with-eval-after-load 'clojure-mode
	  (require 'clojure-mode-extra-font-locking)))

(use-package nrepl-client
  :ensure cider
  :defer t
  :config (setq nrepl-hide-special-buffers t))

(use-package cider-repl
  :ensure cider
  :defer t
  :config
  (progn
    ;; Increase the history size and make it permanent
    (setq cider-repl-history-size 1000
	  cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
	  cider-repl-pop-to-buffer-on-connect nil)

    ;; Disable smartscan for cider-repl
    (add-hook 'cider-repl-mode-hook
	      (lambda ()
		(smartscan-mode -1)))))

;; PANDOC
;; Requires: pandoc
(use-package pandoc-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    (setq org-pandoc-output-format 'odt)))

;; RAINBOW DELIMITERS
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init (dolist (hook '(text-mode-hook prog-mode-hook))
	  (add-hook hook #'rainbow-delimiters-mode)))

;; TRAMP
(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-default-method "ssh"
	  tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
    (add-to-list 'backup-directory-alist
		 (cons tramp-file-name-regexp nil))))

;; AGGRESSIVE INDENT
(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

;; HUNGRY DELETE
(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode))

;; ELFEED
(use-package elfeed
  :ensure t
  :defer t
  :bind (("<f5>" . elfeed))
  :config
  (progn
    (setq elfeed-feeds
	  '(("http://kmandla.wordpress.com/feed/" blog)
	    ("http://inconsolation.wordpress.com/feed/" blog)
	    ("http://planet.emacsen.org/atom.xml" emacs)
	    ("http://planet.clojure.in/atom.xml" clojure)
	    ("http://feeds.feedburner.com/disclojure?format=xml" clojure)
	    ("http://flashstrap.blogspot.com/feeds/posts/default" music)))

    ;; Elfeed: mark all feed as read
    (require 'elfeed-search)

    (defun elfeed-mark-all-as-read ()
      "Mark all fees as read."
      (interactive)
      (call-interactively 'mark-whole-buffer)
      (elfeed-search-untag-all-unread))

    (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)))

;; POST-MODE
(use-package post-mode
  :defer t
  :init
  (progn
    (autoload 'post-mode "post" "mode for e-mail" t)
    (add-to-list 'auto-mode-alist
		 '("\\.*mutt-*\\|.article\\|\\.followup"
		   . post-mode))))

;; COMPANY-MODE
(use-package company
  :ensure t
  :defer t
  :idle (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    (setq company-tooltip-align-annotations t
	  ;; Easy navigation to candidates with M-<n>
	  company-show-numbers t)))

;; Company for AUCTeX
(use-package company-auctex
  :ensure t
  :defer t
  :init (company-auctex-init)
  :config (add-hook 'LaTeX-mode-hook 'company-mode))

;; Company for math symbols
(use-package company-math
  :ensure t
  :defer t
  :config
  (progn
    ;; local configuration for TeX modes
    (defun my-latex-mode-setup ()
      "Add company-math backends."
      (setq-local company-backends
		  (append '(company-math-symbols-latex
			    company-math-symbols-unicode
			    company-latex-commands)
			  company-backends)))
    (add-hook 'TeX-mode-hook 'my-latex-mode-setup)))

;; UNDO-TREE
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; PDF-TOOLS
;; Requires: https://github.com/politza/pdf-tools
(use-package pdf-tools
  :defer t
  :init (pdf-tools-install)
  :config
  (progn
    (defun pdf-outline-imenu-ido ()
      (interactive)
      (let* ((outline (pdf-outline-imenu-create-index-flat))
	     (key (ido-completing-read
		   "Outline: "
		   (mapcar 'car outline)
		   nil t nil 'imenu--history-list)))
	(imenu (assoc key outline))))

    (global-set-key (kbd "C-M-i") 'pdf-outline-imenu-ido)

    ;; No large file warning
    (setq large-file-warning-threshold nil)))

;; JS2-MODE
(use-package js2-mode
  :ensure t
  :defer t
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

;; CSS-MODE
(use-package css-mode
  :defer t
  :config
  (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;; CSS-ELDOC
(use-package css-eldoc
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

;; PHP-MODE
(use-package php-mode
  :ensure t
  :defer t)

;; FLYCHECK
;; Requires: chktex
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (progn
    (setq-default flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-completion-system 'ido
	  flycheck-display-errors-function
	  #'flycheck-pos-tip-error-messages)

    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic)))

(use-package flycheck-pos-tip
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

;; FLYSPELL MODE
;; Requires: aspell, aspell-in, aspell-en
(use-package ispell
  :defer t
  :config
  (progn
    (setq ispell-program-name (executable-find "aspell")
	  ispell-dictionary "italiano"
	  ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available. Install aspell."))))

(use-package flyspell
  :defer t
  :config
  (progn
    (setq flyspell-use-meta-tab nil
	  ;; Make Flyspell less chatty
	  flyspell-issue-welcome-flag nil
	  flyspell-issue-message-flag nil)

    (global-set-key (kbd "C-c I")
		    (lambda()(interactive)
		      (ispell-change-dictionary "italiano")
		      (flyspell-buffer)))

    (global-set-key (kbd "C-c E")
		    (lambda()(interactive)
		      (ispell-change-dictionary "british")
		      (flyspell-buffer)))

    ;; Free C-M-i for completion
    (define-key flyspell-mode-map "\M-\t" nil)))

;; PARADOX
(use-package paradox
  :ensure t
  :defer t
  :bind (("<f4>" . paradox-list-packages)
	 ("S-<f4>" . paradox-upgrade-packages))
  :config
  (setq paradox-github-token t ; Don't ask for a token, please
	;; No async for now
	paradox-execute-asynchronously nil)

  ;; Don't need paradox report
  (remove-hook 'paradox-after-execute-functions #'paradox--report))

;; SX
(use-package sx
  :ensure t
  :defer t)

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

;; BUG-REFERENCE
;; See: http://www.lunaryorn.com/2014/12/23/bug-reference-mode.html
(use-package bug-reference
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
	       (add-hook 'text-mode-hook #'bug-reference-mode)))

;; ELDOC
(use-package eldoc
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;; EMACSSHOT
(use-package emacsshot
  :ensure t
  :bind (("<print>" . emacsshot-snap-frame))
  :defer t)

(provide '05-modes.el)

;;; 05-modes.el ends here
