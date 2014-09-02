;; modes.el
;;
;; This file stores the configurations of every mode I use.

;; hungry-delete-mode
;; http://endlessparentheses.com/hungry-delete-mode.html
(unless (fboundp 'hungry-delete-mode)
  (package-install 'hungry-delete))

(require 'hungry-delete)
(global-hungry-delete-mode)

;; turn on guru-mode
;; see https://github.com/bbatsov/guru-mode
(unless (fboundp 'guru-mode)
  (package-install 'guru-mode))

(require 'guru-mode)
(guru-global-mode +1)

;; turn on smartscan
;; see: https://github.com/mickeynp/smart-scan
(unless (fboundp 'smartscan)
  (package-install 'smartscan))

(global-smartscan-mode 1)

;; minor mode to hide the mode line
;; (see http://bzg.fr/emacs-hide-mode-line.html)
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

;; if you want to hide the mode-line in every buffer by default
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; minor mode for 'override' keybindings
;; see comments here: http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(define-key my-keys-minor-mode-map (kbd "M-h") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "M-a") 'custom/backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-e") 'custom/forward-paragraph)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; turn it off in the minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; DIRED SETUP
(require 'dired)
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
(put 'dired-find-alternate-file 'disabled nil)

;; auto refresh buffers
(global-auto-revert-mode 1)

;; also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; group directories first
(setq dired-listing-switches "--group-directories-first -lah")

;; omit hidden files by default (C-x M-o to show them)
(require 'dired-x)
(setq-default dired-omit-files-p t)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; PO-MODE SETUP
(add-to-list 'load-path "~/.emacs.d/el-get/po-mode")
(require 'po-mode)
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; SCHEME SETUP
;; associate Scheme with GNUGuile
;; required packages:
;; guile-2.0
(setq scheme-program-name "guile")
;; parenthesis and syntax highlighting
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; SLIME SETUP
;; get slime to associate with sbcl
;; the path MAY be emacs or emacs24, depending on build
;; required packages:
;; sbcl, slime, sbcl-doc, cl-clx-sbcl,
;; cl-ppcre, autoconf, texinfo, cl-swank
(setq slime-backend "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
(load-file "/usr/share/emacs/site-lisp/slime/slime.el")
;;(slime-setup '(slime-fancy))
(slime-setup)

;; ORG-MODE SETUP
;; get latest org here: git clone git://orgmode.org/org-mode.git
(add-to-list 'load-path (expand-file-name "~/githubs/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)

;; Org-mode keys
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Org-mode log DONE tasks
(setq org-log-done 'time)

;; Org-mode: update parent nodes when child is removed
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

;; completion with ido
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

;; customized agenda view
(setq org-agenda-custom-commands
            '(("g" "Agenda and giulia-tagged tasks"
               ((agenda "")
                (tags-todo "giulia")
                (tags "giulia")))
              ("m" "Agenda and manuel-tagged tasks"
               ((agenda "")
                (tags-todo "manuel")
                (tags "manuel")))))

;; ORG2BLOG SETUP
;; required packages:
;; bzr
(unless (fboundp 'xml-rpc)
  (package-install 'xml-rpc))

(unless (fboundp 'metaweblog)
  (package-install 'metaweblog))

(unless (fboundp 'org2blog)
  (package-install 'org2blog))

(unless (fboundp 'htmlize)
  (package-install 'htmlize))

(add-to-list 'load-path "~/.emacs.d/elpa/metaweblog")
(add-to-list 'load-path "~/.emacs.d/elpa/xml-rpc")
(add-to-list 'load-path "~/.emacs.d/elpa/org2blog")
(add-to-list 'load-path "~/.emacs.d/elpa/htmlize")

(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
           '(("informatica.boccaperta.com"

              :url "http://informatica.boccaperta.com/xmlrpc.php"
              :username "manuel")))

(setq org2blog/wp-use-sourcecode-shortcode 't)
(setq org2blog/wp-sourcecode-default-params nil)

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

;; E-SHELL SETUP
;; clear eshell buffer
;; (see http://www.khngai.com/emacs/eshell.php)
(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t)

;; MAGIT SETUP
(unless (fboundp 'magit)
  (package-install 'magit))

(require 'magit)

;; fullscreen magit-status
;; see http://whattheemacsd.com/setup-magit.el-01.html
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
;; required packages:
;; texlive-latex-base, texlive-latex-recommended, latexmk,
;; texlive-latex-extra, texlive-fonts-recommended,
;; texlive-generic-recommended
(unless (fboundp 'auctex)
  (package-install 'auctex))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase mode" t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; prettify symbols
(unless (fboundp 'latex-pretty-symbols)
  (package-install 'latex-pretty-symbols))

(require 'latex-pretty-symbols)

;; Make RefTeX faster
(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)
(setq reftex-plug-into-AUCTeX t)

;; Make RefTeX work with Org-Mode
;; use 'C-c (' instead of 'C-c [' because the latter is already
;; defined in orgmode to the add-to-agenda command.
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
  (file-exists-p (buffer-file-name))
  (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c (") 'reftex-citation))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;; use latexmk for compilation by default
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("latexmk" "latexmk -synctex=1 -shell-escape -pdf %s" TeX-run-TeX nil t :help "Process file with latexmk"))
)
(eval-after-load "tex"
  '(add-to-list 'TeX-command-list '("xelatexmk" "latexmk -synctex=1 -shell-escape -xelatex %s" TeX-run-TeX nil t :help "Process file with xelatexmk"))
)

(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

; add LaTeX to the list of languages Org-babel will recognize
(require 'ob-latex)

;; add LaTeX to a list of languages that raise noweb-type errors
(add-to-list 'org-babel-noweb-error-langs "latex")

;; use ebib for BibTeX
(unless (fboundp 'ebib)
  (package-install 'ebib))

;; use ebib links in org-mode
(org-add-link-type "ebib" 'ebib)

;; ERC SETUP
(defun read-lines (filePath)
"Return a list of lines of a file at filePath."
(with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n")))

(let ((acc (read-lines "~/.my-erc-account")))
  (setq erc-nick (car acc)))
