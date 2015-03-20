;;; custom.el --- Part of my Emacs configuration

;;; Commentary:
;; First file to be loaded when Emacs starts.

;;; Code:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "/home/manuel/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "3c093ea152d7185cc78b61b05e52648c6d2fb0d8579c2119d775630fa459e0be" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(font-lock-maximum-decoration (quote ((dired-mode) (t . t))))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/org/organizer.org")))
 '(package-selected-packages
   (quote
    (yasnippet whitespace-cleanup-mode esup sx langtool unkillable-scratch web-mode popup
               (symbol-value
                (quote t))
               (symbol-value
                (quote t))
               (symbol-value
                (quote t))
               (symbol-value
                (quote t))
               (symbol-value
                (quote t))
               (symbol-value
                (quote t))
               (progn t elisp--witness--lisp)
               bind-key synosaurus lice dash smart-mode-line flycheck interleave visual-fill-column aggressive-fill-paragraph clojure-mode rich-minority helm ibuffer-vc emacs-neotree transpose-frame
               (progn t elisp--witness--lisp)
               org-tree-slide org-tree-slides
               (progn t elisp--witness--lisp)
               persistent-soft latest-clojure-libraries dired+ solarized-theme js2-mode pandoc-mode slime projectile ido-ubiquitous
               (progn t elisp--witness--lisp)
               csv-mode ignoramus php-mode org typo-mode paradox zop-to-char elisp--witness--lisp rainbow-mode flycheck-package elisp-slime-nav highlight-symbol anzu bookmark+ company pdf-tools wgrep-ag visual-regexp use-package undo-tree smex slime-company request rainbow-delimiters popwin page-break-lines org2blog multiple-cursors multifiles magit macrostep latex-extra iedit ido-vertical-mode ido-load-library ibuffer-projectile hungry-delete htmlize gitignore-mode gitconfig-mode gitattributes-mode geiser font-utils flycheck-pos-tip flx-isearch flx-ido f expand-region emacsshot elfeed easy-kill diff-hl css-eldoc company-math clojure-mode-extra-font-locking cider camcorder browse-kill-ring auctex-latexmk aggressive-indent ag adaptive-wrap)))
 '(paradox-github-token t)
 '(recentf-auto-cleanup 300)
 '(synosaurus-choose-method (quote ido)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; custom.el ends here
