;;; 04-modes.el --- Part of my Emacs configuration

;;; Commentary:
;; This file stores the configurations of every mode I use.

;;; Code:
;; Turn on hungry-delete-mode
;; See: http://endlessparentheses.com/hungry-delete-mode.html
(use-package hungry-delete
  :ensure t
  :init (global-hungry-delete-mode))

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
  :config
  (progn
    ;; Power up dired
    (require 'dired-x)
    ;; Always revert Dired buffers on revisiting
    (setq dired-auto-revert-buffer t
	  dired-listing-switches "--group-directories-first -lah"
	  ;; Also auto refresh dired, but be quiet about it
	  global-auto-revert-non-file-buffers t
	  auto-revert-verbose nil)))

(use-package dired-x
  :defer t
  :config
  (progn
    ;; Omit hidden files by default (C-x M-o to show them)
    (setq-default dired-omit-files-p t)
    (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))))

;; Ignore uninteresting files
(use-package ignoramus
  :ensure t
  :init (ignoramus-setup))

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

;; Track recent files
(use-package recentf
  :init (recentf-mode)
  :bind (("C-x C-r" . ido-recentf-open))
  :config
  (setq recentf-max-saved-items 200
	recentf-max-menu-items 15
	recentf-auto-cleanup 300
	recentf-exclude (list "/\\.git/.*\\'" ; Git contents
			      "/elpa/.*\\'" ; Package files
			      ;; And all other kinds of boring files
			      #'ignoramus-boring-p)))

;; PO-MODE
;; (add-to-list 'load-path "~/.emacs.d/el-get/po-mode")
;; (require 'po-mode)
;; (setq auto-mode-alist
;;       (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
;; (autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; SCHEME
;; Requires: guile-2.0
(use-package geiser
  :ensure t
  :defer t
  :config
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
(add-to-list 'load-path "~/.emacs.d/various")
(autoload 'org-present "org-present" nil t)

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images t t)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)))

;; ORG2BLOG
(use-package metaweblog
  :ensure t)

(use-package xml-rpc
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package org2blog
  :ensure t
  :init (require 'org2blog-autoloads)
  :config
  (progn
    (setq org2blog/wp-blog-alist
	  '(("informatica.boccaperta.com"

	     :url "http://informatica.boccaperta.com/xmlrpc.php"
	     :username "manuel")))))

;; DOC-VIEW-MODE
(use-package doc-view
  :defer t
  :config
  (progn
    (setq doc-view-continuous t
	  ;; No large file warning
	  large-file-warning-threshold nil)))

;; ESHELL
(use-package eshell
  :defer t
  :bind (("<f1>" . eshell))
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
  :bind (("<f2>" . shell)))

;; MAGIT SETUP
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

    (defun custom-kill-buffers (regexp)
      "Kill buffers matching REGEXP without asking for confirmation."
      (interactive "sKill buffers matching this regular expression: ")
      (cl-letf (((symbol-function 'kill-buffer-ask)
		 (lambda (buffer) (kill-buffer buffer))))
	(kill-matching-buffers regexp)))

    (defun magit-quit-session ()
      "Restore the previous window configuration and kill the magit buffer."
      (interactive)
      (custom-kill-buffers "^\\*magit")
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
    (setq LaTeX-babel-hyphen nil)
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))) ; Easy math input

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

;; Plug reftex into bib-cite
(use-package bib-cite
  :defer t
  :config (setq bib-cite-use-reftex-view-crossref t))

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
(use-package clojure-mode
  :ensure cider
  :defer t
  :config
  (progn
    (add-hook 'clojure-mode-hook #'paredit-mode)
    (add-hook 'clojure-mode-hook #'cider-mode)))

;; Extra font-locking for Clojure
(use-package clojure-mode-extra-font-locking
  :ensure clojure-mode-extra-font-locking
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
  ;; Increase the history size and make it permanent
  (setq cider-repl-history-size 1000
	cider-repl-history-file (locate-user-emacs-file "cider-repl-history")
	cider-repl-pop-to-buffer-on-connect nil))

;; PANDOC
;; Requires: pandoc
(use-package pandoc-mode
  :ensure t
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

;; TRAMP SETUP
(add-to-list 'load-path "~/emacs/tramp/lisp/")
(require 'tramp)

(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;; AGGRESSIVE INDENT
(use-package aggressive-indent
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

;; ELFEED
(use-package elfeed
  :ensure t
  :bind (("<f5>" . elfeed))
  :config
  (progn
    (setq elfeed-feeds
	  '(("http://kmandla.wordpress.com/feed/" blog)
	    ("http://inconsolation.wordpress.com/feed/" blog)
	    ("http://planet.emacsen.org/atom.xml" emacs)
	    ("http://endlessparentheses.com/atom.xml" emacs)
	    ("http://www.masteringemacs.org/feed/" emacs)
	    ("http://oremacs.com/atom.xml" emacs)
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
(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist
             '("\\.*mutt-*\\|.article\\|\\.followup"
	       . post-mode))

;; COMPANY-MODE
(use-package company
  :ensure t
  :defer t
  :config
  (progn
    (setq company-show-numbers t)

    ;; company for Cider
    (add-hook 'cider-repl-mode-hook 'company-mode)
    (add-hook 'cider-mode-hook 'company-mode)

    ;; company for Elisp
    (add-hook 'emacs-lisp-mode-hook 'company-mode)))

;; Company for AUCTeX
(use-package company-auctex
  :ensure t
  :defer t
  :init (company-auctex-init)
  :config (add-hook 'LaTeX-mode-hook 'company-mode))

;; Company for math
(use-package company-math
  :ensure t
  :defer t
  :config
  (progn
    ;; local configuration for TeX modes
    (defun my-latex-mode-setup ()
      "Add company-math backends."
      (setq-local company-backends
		  (append '(company-math-symbols-latex company-latex-commands)
			  company-backends)))

    (add-hook 'TeX-mode-hook 'my-latex-mode-setup)))

;; UNDO-TREE SETUP
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; PDF-TOOLS
;; Requires: https://github.com/politza/pdf-tools
(pdf-tools-install)

;; WEB-MODE
(use-package web-mode
  :ensure t
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))))

;; JS2-MODE
(use-package js2-mode
  :ensure t
  :defer t
  :config (add-hook 'js-mode-hook 'js2-minor-mode))

;; FLYCHECK SETUP
;; Requires: chktex
(use-package flycheck
  :ensure t
  :idle (global-flycheck-mode)
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

;; FLYSPELL MODE SETUP
;; Requires: aspell, aspell-in, aspell-en
(use-package flyspell
  :defer t
  :config
  (progn
    (setq flyspell-use-meta-tab nil
	  ;; Make Flyspell less chatty
	  flyspell-issue-welcome-flag nil
	  flyspell-issue-message-flag nil)

    (defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
      "If RUN-TOGETHER is true, spell check the CamelCase words."
      (let (args)
	(cond
	 ((string-match  "aspell$" ispell-program-name)
	  (setq args (list "--sug-mode=ultra" "--lang=it_IT"))
	  (if RUN-TOGETHER
	      (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
	 ((string-match "hunspell$" ispell-program-name)
	  (setq args nil)))
	args
	))

    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-dictionary "italiano"))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-local-dictionary "it_IT")
      (setq ispell-local-dictionary-alist
	    '(("it_IT" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
     (t (setq ispell-program-name nil)))

    ;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to
    ;; the ispell process when "ispell-word" is called.
    ;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
    (setq ispell-extra-args (flyspell-detect-ispell-args t))

    (defadvice ispell-word (around my-ispell-word activate)
      "Take care of extra args."
      (let ((old-ispell-extra-args ispell-extra-args))
	(ispell-kill-ispell t)
	(setq ispell-extra-args (flyspell-detect-ispell-args))
	ad-do-it
	(setq ispell-extra-args old-ispell-extra-args)
	(ispell-kill-ispell t)
	))

    (global-set-key (kbd "C-c I")
		    (lambda()(interactive)
		      (ispell-change-dictionary "italiano")
		      (flyspell-buffer)))
    (global-set-key (kbd "C-c E")
		    (lambda()(interactive)
		      (ispell-change-dictionary "english")
		      (flyspell-buffer)))))

;; PARADOX SETUP
(use-package paradox
  :ensure t
  :bind (("<f4>" . paradox-list-packages))
  :config
  ;; Don't ask for a token, please
  (setq paradox-github-token t))

;;; 04-modes.el ends here
