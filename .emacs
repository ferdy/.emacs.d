;; INTERFACE SETUP
;; font Anonymous Pro, fallback DejaVu
(set-face-attribute 'default nil
                    :family "Anonymous Pro"
                    :height 115
                    :weight 'normal
                    :width 'normal)

(when (functionp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 11.4
                               :weight 'normal)))

;; turn off blinking cursor
(blink-cursor-mode 0)

;; turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; better backspacing
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; C^n adds new line when at the end of a line
(setq next-line-add-newlines t)

;; scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; disable annoying prompts
(fset 'yes-or-no-p 'y-or-n-p)
(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

;; disable splash screen
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; disable scratch buffer message
(setq initial-scratch-message nil)

;; tooltips in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)

;; turn on ido-mode for better buffers switching
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

;; minor mode to hide the mode line (see http://bzg.fr/emacs-hide-mode-line.html)
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
  ;; Apparently force-mode-line-update is not always enough to
  ;; redisplay the mode-line
  (redraw-display)
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message "Hidden Mode Line Mode enabled.")))

;; If you want to hide the mode-line in every buffer by default
(add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; custom keybindings activated with C^x t (see http://endlessparentheses.com/the-toggle-map-and-wizardry.html)
(define-prefix-command 'toggle-map)
;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
(define-key ctl-x-map "t" 'toggle-map)
(define-key toggle-map "v" 'visual-line-mode)
(define-key toggle-map "c" 'column-number-mode)
(define-key toggle-map "l" 'linum-mode)
(define-key toggle-map "h" 'hidden-mode-line-mode)

;; set the directory where all backup and autosave files will be saved
(defvar backup-dir "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,backup-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,backup-dir t)))

;; set solarized theme
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(require 'solarized-dark-theme)
(load-theme 'solarized-dark t)

;; DIRED SETUP
(require 'dired)
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;;(add-to-list 'load-path "~/.emacs.d/el-get/dired+")
;;(require 'dired+)

;; auto refresh buffers
(global-auto-revert-mode 1)

;; also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; PACKAGES SETUP
(require 'package)
;; add the original Emacs Lisp Package Archive
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; add the user-contributed repository
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
;; add Melpa
(add-to-list 'package-archives
'("melpa" . "http://melpa.milkbox.net/packages/") t)

;; packages installed via package-el are activated
;; AFTER .emacs is loaded. So I need to call initialize
;; to be able to load custom theme
;;(package-initialize)

;; use El-Get to sync repos and dependencies.
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; PO-MODE SETUP
(add-to-list 'load-path "~/.emacs.d/el-get/po-mode")
(require 'po-mode)
(setq auto-mode-alist
      (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))
(autoload 'po-mode "po-mode" "Major mode for translators to edit PO files" t)

;; SCHEME SETUP
;; associate Scheme with GNUGuile
(setq scheme-program-name "guile")
;; parenthesis and syntax highlighting
(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; SLIME SETUP
;;get slime to associate with sbcl
;;the path MAY be emacs or emacs24...depending on build
(setq slime-backend "/usr/share/common-lisp/source/slime/swank-loader.lisp")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
(load-file "/usr/share/emacs/site-lisp/slime/slime.el")
;;(slime-setup '(slime-fancy))
(slime-setup)

;; PER-FILE SETUP
;; C-specific Indentation
(setq c-default-style "linux"
      c-basic-offset 4)

;; delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; ORG-MODE SETUP
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

;; Org-mode font lock
(global-font-lock-mode 1)

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
(add-to-list 'load-path "~/.emacs.d/el-get/metaweblog")
(add-to-list 'load-path "~/.emacs.d/el-get/xml-rpc-el")
(add-to-list 'load-path "~/.emacs.d/el-get/org2blog")
(add-to-list 'load-path "~/.emacs.d/el-get/htmlize")
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
           '(("informatica.boccaperta.com"

              :url "http://informatica.boccaperta.com/xmlrpc.php"
              :username "manuel")))

(setq org2blog/wp-use-sourcecode-shortcode 't)
(setq org2blog/wp-sourcecode-default-params nil)

;; DOC-VIEW-MODE SETUP
(setq doc-view-continuous t)

;; CUSTOM VARIABLES - DON'T TOUCH
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(background-color "#fdf6e3")
 '(background-mode light)
 '(cursor-color "#657b83")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" default)))
 '(dired-listing-switches "-al --group-directories-first")
 '(fci-rule-color "#383838")
 '(foreground-color "#657b83"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
