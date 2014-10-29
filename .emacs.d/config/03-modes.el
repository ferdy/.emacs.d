;;;; 03-modes.el

;;; This file stores the configurations of every mode I use.

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

;; Minor mode for 'override' keybindings
;; See comments here: http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "M-h") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "M-a") 'custom/backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-e") 'custom/forward-paragraph)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; Turn it off in the minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

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
;; Required packages: guile-2.0
(setq scheme-program-name "guile")
;; Parenthesis and syntax highlighting
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; SLIME SETUP
;; Get slime to associate with sbcl
;; The path MAY be emacs or emacs24, depending on build
;; Required packages:
;; sbcl, slime, sbcl-doc, cl-clx-sbcl,
;; cl-ppcre, autoconf, texinfo, cl-swank
(setq slime-backend "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl")
(load-file "/usr/share/emacs/site-lisp/slime/slime.el")
(slime-setup)

;; ORG-MODE SETUP
(require 'org)

;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Log DONE tasks
(setq org-log-done 'time)

;; Update parent nodes when child is removed
(defun myorg-update-parent-cookie ()
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

;; DOC-VIEW-MODE SETUP
(setq doc-view-continuous t)

;; ESHELL SETUP
;; Clear eshell buffer
;; See http://www.khngai.com/emacs/eshell.php
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t)

;; Eshell history powered by ido
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

;; Run scrips from current working on remote system
(defadvice eshell-gather-process-output (before absolute-cmd (command args) act)
  (setq command (file-truename command)))

;; MAGIT SETUP
(require 'magit)

;; Fullscreen magit-status
;; See http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun custom-kill-buffers (regexp)
  "Kill buffers matching REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: ")
  (flet ((kill-buffer-ask (buffer) (kill-buffer buffer)))
    (kill-matching-buffers regexp)))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (custom-kill-buffers "^\\*magit")
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; AUCTEX SETUP
;; Required packages:
;; texlive-latex-base, texlive-latex-recommended, latexmk,
;; texlive-latex-extra, texlive-fonts-recommended,
;; texlive-generic-recommended, texlive-xetex
;; texlive-lang-italian
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; Make RefTeX faster
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

;; Make RefTeX work with Org-Mode
;; Use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(defun org-mode-reftex-setup ()
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
;; requires in ~/.ercpass the format
;; (setq variable "nickname")
;; (setq variable "password")
(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)

(setq erc-nick gp-nick)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserve-passwords
      `((freenode (,gp-nick . ,gp-pass))))
(setq erc-autojoin-channels-alist '((".*" "#linuxbbq")))

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

;; ELFEED SETUP
(setq elfeed-feeds
      '(("http://kmandla.wordpress.com/feed/" blog)
	("http://inconsolation.wordpress.com/feed/" blog)
	("http://planet.emacsen.org/atom.xml" emacs)
	("http://endlessparentheses.com/atom.xml" emacs)
	("http://www.masteringemacs.org/feed/" emacs)
	("https://www.techdirt.com/techdirt_rss.xml" tech)
	("https://firstlook.org/theintercept/feeds/" tech)
	("http://rt.com/rss/" news)
	("http://feeds.theguardian.com/theguardian/world/rss" news)
	("http://www.wumingfoundation.com/giap/?feed=rss2" blog)
	("http://feeds.feedburner.com/disinfo/oMPh" news)
	("http://flashstrap.blogspot.com/feeds/posts/default" music)
	("http://thaifilmjournal.blogspot.com/feeds/posts/default" cinema)
	("http://www.loveasianfilm.com/feed" cinema)))

;; MU4E SETUP
;; Required packages: mu4e
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)

(setq mu4e-maildir "~/mail")
(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 300)
(setq message-send-mail-function 'smtpmail-send-it)
(setq smtpmail-smtp-server "smtp.boccaperta.com")

;; Set mu4e as default Emacs e-mail program
(setq mail-user-agent 'mu4e-user-agent)
