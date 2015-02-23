;;; 04-modes.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:
;; This file stores the configurations of every mode I use.

;;; Code:

;;; Files
(use-package dired
  :defer t
  :bind (("C-c z" . dired-get-size)
	 ("C-c C" . copy-file-name-to-clipboard))
  :config
  (progn
    ;; Always revert Dired buffers on revisiting
    (setq dired-auto-revert-buffer t
	  dired-listing-switches "-laGh1v --group-directories-first"
	  ;; Also auto refresh dired, but be quiet about it
	  global-auto-revert-non-file-buffers t
	  auto-revert-verbose nil
	  ;; Don't ask about recursive copies
	  dired-recursive-copies 'always)

    ;; Better M-< and M->
    (defun dired-back-to-top ()
      (interactive)
      (beginning-of-buffer)
      (dired-next-line 2))

    (define-key dired-mode-map
      (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

    (defun dired-jump-to-bottom ()
      (interactive)
      (end-of-buffer)
      (dired-next-line -1))

    (define-key dired-mode-map
      (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

    (define-key dired-mode-map "!" 'sudired)

    ;; Use other pane as default destination when copying
    (setq dired-dwim-target t)))

(use-package dired-x
  :defer t
  :config
  (progn
    ;; Omit hidden files by default (C-x M-o to show them)
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))
    ;; Hide omit files messages
    (setq dired-omit-verbose nil)))

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

(use-package hardhat
  :ensure t
  :defer t
  :idle (global-hardhat-mode))

(use-package doc-view
  :defer t
  :config
  (progn
    (setq doc-view-continuous t)))

;; View read-only
(setq view-read-only t)

;;; Translation
(use-package po-mode
  :load-path "el-get/po-mode"
  :defer t
  :init
  (progn
    (setq auto-mode-alist
	  (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))))

;;; Org
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

;;; Shells
(use-package eshell
  :defer t
  :bind (("<f1>" . eshell-here))
  :config
  (progn
    ;; Clear eshell buffer
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
		(local-set-key (kbd "C-c C-h") 'eshell-list-history)))))

(use-package ansi-term
  :defer t
  :bind (("<f2>" . custom/term))
  :init
  (progn
    ;; Default shell is Zsh
    (defun custom/term ()
      "Wrapper for `ansi-term'."
      (interactive)
      (ansi-term "/bin/zsh"))

    ;; Close buffer on exit
    (defun custom/term-exec-hook ()
      (let* ((buff (current-buffer))
             (proc (get-buffer-process buff)))
        (set-process-sentinel
         proc
         `(lambda (process event)
            (if (string= event "finished\n")
                (custom/kill-buffers "^\\*ansi-term"))))))

    (add-hook 'term-exec-hook 'custom/term-exec-hook)))

(use-package shell
  :defer t
  :bind (("S-<f2>" . shell))
  :config
  (progn
    ;; Clear shell buffer
    (defun clear-shell ()
      (interactive)
      (let ((comint-buffer-maximum-size 0))
	(comint-truncate-buffer)))

    (define-key shell-mode-map (kbd "C-l") 'clear-shell)

    ;; Shell buffer maximized
    (add-hook 'shell-mode-hook
              (lambda ()
                (delete-other-windows)))))

;;; Version control
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

;;; LaTeX
;; Requires: texlive-latex-base, texlive-latex-recommended,
;; latexmk, texlive-latex-extra, texlive-fonts-recommended,
;; texlive-generic-recommended, texlive-xetex
;; texlive-lang-italian, cjk-latex, latex-cjk-all,
;; texlive-lang-cjk, texlive-chinese-lang
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

;;; Formatting
(use-package markdown-mode
  :ensure t
  :config
  (progn
    ;; Use Pandoc to process Markdown
    (setq markdown-command "pandoc -s -f markdown -t html5")))

;; Requires: pandoc
(use-package pandoc-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
    (setq org-pandoc-output-format 'odt)))

;;; Remote editing
(use-package tramp
  :defer t
  :config
  (progn
    (setq tramp-default-method "ssh"
          tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"
          auto-save-file-name-transforms nil)
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))))

;;; Web
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

(use-package elfeed
  :ensure t
  :defer t
  :bind (("<f5>" . elfeed))
  :config
  (progn
    (setq elfeed-feeds
	  '(("http://kmandla.wordpress.com/feed/" blog)
            ("http://planet.emacsen.org/atom.xml" emacs)
	    ("http://planet.clojure.in/atom.xml" clojure)
	    ("http://feeds.feedburner.com/disclojure?format=xml" clojure)
	    ("http://flashstrap.blogspot.com/feeds/posts/default" music)
            ("http://jazzfromitaly.blogspot.it/feeds/posts/default" music)
            ("http://www.wumingfoundation.com/giap/?feed=rss2" book)))

    ;; Elfeed: mark all feed as read
    (require 'elfeed-search)

    (defun elfeed-mark-all-as-read ()
      "Mark all fees as read."
      (interactive)
      (call-interactively 'mark-whole-buffer)
      (elfeed-search-untag-all-unread))

    (define-key elfeed-search-mode-map (kbd "R") 'elfeed-mark-all-as-read)))

(use-package sx
  :ensure t
  :defer t)

(use-package sx-question-mode
  :ensure sx
  :defer t
  ;; Display questions in the same window
  :config (setq sx-question-mode-display-buffer-function #'switch-to-buffer))

;;; Completion
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

(use-package company-auctex
  :ensure t
  :defer t
  :init (company-auctex-init)
  :config (add-hook 'LaTeX-mode-hook 'company-mode))

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

;;; Utilities
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

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
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-display-if-noquery))

(use-package bug-reference
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'bug-reference-prog-mode)
               (add-hook 'text-mode-hook #'bug-reference-mode)))

(use-package eldoc
  :defer t
  ;; Enable Eldoc for `eval-expression', too
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package emacsshot
  :ensure t
  :bind (("<print>" . emacsshot-snap-frame))
  :defer t)

(use-package camcorder
  :ensure t
  :defer t
  :init (setq camcorder-window-id-offset -2))

(use-package archive-mode
  :defer t
  :config (add-to-list
           'auto-mode-alist '("\\.\\(cbr\\)\\'" . archive-mode)))

(use-package proced
  :defer t
  :config
  (progn
    ;; Auto-update proced buffer
    (defun proced-settings ()
      (proced-toggle-auto-update 1))

    (add-hook 'proced-mode-hook 'proced-settings)))

(use-package csv-mode
  :ensure t
  :defer t)

;;; Project Management
(use-package projectile
  :ensure t
  :defer t
  :init (projectile-global-mode)
  :idle (projectile-cleanup-known-projects)
  :idle-priority 10
  :config
  (progn
    (setq projectile-completion-system 'ido
          projectile-find-dir-includes-top-level t)

    ;; Replace Ack with Ag in Projectile commander
    (def-projectile-commander-method ?a
      "Find ag on project."
      (call-interactively 'projectile-ag))))

;; Group buffers by Projectile project
(use-package ibuffer-projectile
  :ensure t
  :defer t
  :init
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphapbetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;; Syntax checking
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

(use-package flycheck-package
  :ensure t
  :defer t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

;;; Spell checking
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

;;; Programming
;;; Clojure
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
          cider-repl-pop-to-buffer-on-connect nil)))

(use-package latest-clojure-libraries
  :ensure t
  :defer t)

;;; Scheme
;; Requires: guile-2.0
(use-package geiser
  :ensure t
  :defer t
  :init
  (progn
    (setq scheme-program-name "guile")
    (setq geiser-impl-installed-implementations '(guile))))

;;; Slime
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

;;; Web
(use-package web-mode
  :ensure t
  :defer t
  :config
  (setq web-mode-markup-indent-offset 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\(?:on\\)?\\'"
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

(use-package css-mode
  :defer t
  :config
  (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package css-eldoc
  :ensure t
  :commands (turn-on-css-eldoc)
  :init (add-hook 'css-mode-hook #'turn-on-css-eldoc))

(use-package php-mode
  :ensure t
  :defer t)

(provide '04-modes)

;;; 04-modes.el ends here
