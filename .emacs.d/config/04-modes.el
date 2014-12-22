;;; 04-modes.el --- Part of my Emacs configuration

;;; Commentary:
;; This file stores the configurations of every mode I use.

;;; Code:
;; Turn on hungry-delete-mode
;; See: http://endlessparentheses.com/hungry-delete-mode.html
(global-hungry-delete-mode)

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

;; DIRED SETUP
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Group directories first
(setq dired-listing-switches "--group-directories-first -lah")

;; Omit hidden files by default (C-x M-o to show them)
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; Reuse buffer for directories
(require 'dired+)
(diredp-toggle-find-file-reuse-dir 1)
(setq diredp-hide-details-initially-flag nil)
(setq diredp-hide-details-propagate-flag nil)

;; PO-MODE SETUP
(add-to-list 'load-path "~/.emacs.d/el-get/po-mode")
(require 'po-mode)
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; SCHEME SETUP
;; Associate Scheme with GNUGuile
;; Requires: guile-2.0
(setq scheme-program-name "guile")

;; Use this for Chicken Scheme instead of Guile
;;(setq scheme-program-name "csi -:c")

;; Parenthesis and syntax highlighting
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; SLIME SETUP
;; Get slime to associate with sbcl
;; The path MAY be emacs or emacs24, depending on build
;; Requires: sbcl, slime, sbcl-doc, cl-clx-sbcl,
;; cl-ppcre, autoconf, texinfo, cl-swank
(setq slime-backend "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl")
(load-file "/usr/share/emacs/site-lisp/slime/slime.el")
(slime-setup)

;; ORG-MODE SETUP
(require 'org)

;; Fontify src
(setq org-src-fontify-natively t)

;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Log DONE tasks
(setq org-log-done 'time)

;; Update parent nodes when child is removed
(defun myorg-update-parent-cookie ()
  "Update parent nodes when child is removed."
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

;; Completion with ido
(setq org-completion-use-ido t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (C .t)
   (awk .t)
   (lisp .t)
   (python .t)
   (scheme .t)))

;; Customized agenda view
(setq org-agenda-custom-commands
      '(("g" "Agenda and giulia-tagged tasks"
	 ((agenda "")
	  (tags-todo "giulia")
	  (tags "giulia")))
	("m" "Agenda and manuel-tagged tasks"
	 ((agenda "")
	  (tags-todo "manuel")
	  (tags "manuel")))))

;; Turn off preamble and postamble in HTML export
(setq org-html-preamble nil)
(setq org-html-postamble nil)
(setq org-export-html-style-default "")
(setq org-export-html-style-include-default nil)

;; Auto insert custom text upon opening an org file
(auto-insert-mode)
(setq auto-insert-query nil)
(eval-after-load 'autoinsert
  '(define-auto-insert '("\\.org\\'" . "Org skeleton")
     '(
       "Short description: "
       "#+STARTUP: showall\n"
       > _ \n \n)))

;; ORG-PRESENT SETUP
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

;; ORG2BLOG SETUP
(add-to-list 'load-path "~/.emacs.d/el-get/metaweblog")
(add-to-list 'load-path "~/.emacs.d/el-get/xml-rpc")

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("informatica.boccaperta.com"

	 :url "http://informatica.boccaperta.com/xmlrpc.php"
	 :username "manuel")))

(setq org2blog/wp-use-sourcecode-shortcode 't)
(setq org2blog/wp-sourcecode-default-params nil)

;; DOC-VIEW-MODE SETUP
(setq doc-view-continuous t)

;; No large file warning
(setq large-file-warning-threshold nil)

;; ESHELL SETUP
(require 'eshell)

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
	    (smartscan-mode -1)))

;; MAGIT SETUP
(require 'magit)

;; Shut up, Magit!
(setq magit-save-some-buffers 'dontask
      magit-stage-all-confirm nil
      magit-unstage-all-confirm nil
      ;; Except when you ask something useful…
      magit-set-upstream-on-push t
      ;; Use IDO for completion
      magit-completing-read-function #'magit-ido-completing-read)

;; Auto-revert files after Magit operations
(magit-auto-revert-mode)
(setq magit-auto-revert-mode-lighter "")

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

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; AUCTEX SETUP
;; Requires: ;; texlive-latex-base, texlive-latex-recommended,
;; latexmk, texlive-latex-extra, texlive-fonts-recommended,
;; texlive-generic-recommended, texlive-xetex
;; texlive-lang-italian, cjk-latex, latex-cjk-all
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(add-hook 'LaTeX-mode-hook #'latex-extra-mode) ; extra commands and keys

;; Make RefTeX faster
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

;; Make RefTeX work with Org-Mode
;; Use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(defun org-mode-reftex-setup ()
  "Make RefTeX work with Org-Mode."
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; Use latexmk for compilation by default
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk"))
  )
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
  )

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "xelatexmk")))

;; Set default engine: xetex
(setq-default TeX-engine 'xetex)

;; Add LaTeX to the list of languages Org-babel will recognize
(require 'ob-latex)

;; Add LaTeX to a list of languages that raise noweb-type errors
(add-to-list 'org-babel-noweb-error-langs "latex")

;; Use ebib links in org-mode
(org-add-link-type "ebib" 'ebib)

;; Add latex-extra
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

;; Turn on adaptive wrap for latex-mode
(eval-after-load 'tex-mode
  '(progn
     (add-hook 'latex-mode-hook #'adaptive-wrap)))

;; ERC SETUP
;; Requires in ~/.ercpass the format
;; (setq variable "nickname")
;; (setq variable "password")
(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)

(setq erc-nick gp-nick)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserve-passwords
      `((freenode (,gp-nick . ,gp-pass))))

;; ELECTRIC-PAIR-MODE SETUP
(electric-pair-mode +1)

(setq electric-pair-pairs '((?\" . ?\")
			    (?\{ . ?\})))

;; CLOJURE MODE SETUP
(require 'clojure-mode-extra-font-locking)

;; CIDER SETUP
;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide the *nrepl-connection* and *nrepl-server* when C-x b
;; (they are available in C-x C-b)
(setq nrepl-hide-special-buffers t)

;; Prefer local resources to remote (tramp) ones when both are available
(setq cider-prefer-local-resources t)

;; Prevent the auto-display of the REPL buffer in a separate
;; window after connection is established
(setq cider-repl-pop-to-buffer-on-connect nil)

;; PANDOC SETUP
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(setq org-pandoc-output-format 'odt)

;; RAINBOW DELIMITERS SETUP
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; TRAMP SETUP
(add-to-list 'load-path "~/emacs/tramp/lisp/")
(require 'tramp)

(setq tramp-default-method "ssh")
(setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;; AGGRESSIVE INDENT SETUP
(global-aggressive-indent-mode 1)

;; Disable for cider-repl
(add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode)

;; ELFEED SETUP
(setq elfeed-feeds
      '(("http://kmandla.wordpress.com/feed/" blog)
	("http://inconsolation.wordpress.com/feed/" blog)
	("http://planet.emacsen.org/atom.xml" emacs)
	("http://endlessparentheses.com/atom.xml" emacs)
	("http://www.masteringemacs.org/feed/" emacs)
	("http://oremacs.com/atom.xml" emacs)
	("http://flashstrap.blogspot.com/feeds/posts/default" music)))

;; POST-MODE SETUP
(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist
             '("\\.*mutt-*\\|.article\\|\\.followup"
	       . post-mode))

;; COMPANY-MODE SETUP
;; Easy navigation to candidates with M-<n>
(setq company-show-numbers t)

;; company for AUCTeX
(require 'company-auctex)
(company-auctex-init)
(add-hook 'LaTeX-mode-hook 'company-mode)

;; local configuration for TeX modes
(defun my-latex-mode-setup ()
  "Add company-math backends."
  (setq-local company-backends
              (append '(company-math-symbols-latex company-latex-commands)
                      company-backends)))

(add-hook 'TeX-mode-hook 'my-latex-mode-setup)

;; company for Cider
(add-hook 'cider-repl-mode-hook 'company-mode)
(add-hook 'cider-mode-hook 'company-mode)

;; company for Elisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; MU4E SETUP
;; Requires: mu4e, gnutls-bin
;; Optional: nullmailer
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

;; Set mu4e as default Emacs e-mail program
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-maildir "~/mail")
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)

;; set this to nil so signature is not included by default
;; you can include in message with C-c C-w
(setq mu4e-compose-signature-auto-include 't)
(setq mu4e-compose-signature
      (concat
       "Manuel Uberti\n"
       "Email: manuel@boccaperta.com\n"
       "Web: informatica.boccaperta.com"
       "\n"))

;; Smtp setup
(require 'smtpmail)
(require 'starttls)

;; Send mail using nullmailer
;; Check /etc/nullmailer/remotes for smtp settings
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; Don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; Show images
(setq mu4e-show-images t)

;; Show full addresses in view message (instead of just names)
;; toggle per name with M-RET
(setq mu4e-view-show-addresses 't)

;; UNDO-TREE SETUP
(global-undo-tree-mode)

;; PDF-TOOLS
;; Requires: https://github.com/politza/pdf-tools
(pdf-tools-install)

;; WEB-MODE
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode))

;; JS2-MODE
(add-hook 'js-mode-hook 'js2-minor-mode)

;; FLYCHECK SETUP
;; Requires: chktex
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; FLYSPELL MODE SETUP
;; Requires: aspell, aspell-in, aspell-en
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

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
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
		  (ispell-change-dictionary "english")
		  (flyspell-buffer)))

;; PARADOX SETUP
(setq paradox-execute-asynchronously t)

;;; 04-modes.el ends here
