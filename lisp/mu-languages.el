;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for languages and translations.

;;; Code:

;;; Translation
(use-package po-mode ; Manage .po files
  :load-path "various"
  :mode "\\.po\\'"
  :no-require t
  :config (setq po-keep-mo-file t))

;;; Spell checking and dictionaries
(use-package ispell ; Word correction
  :defer t
  :config (progn
            (setq ispell-program-name (executable-find "aspell")
                  ispell-dictionary "italiano"
                  ispell-choices-win-default-height 5)

            (unless ispell-program-name
              (warn "No spell checker available. Install aspell."))

            ;; Ispell with Abbrev for auto-correcting spelling mistakes
            (bind-key "C-i" #'mu-ispell-word-then-abbrev ctl-x-map)

            (defun mu-ispell-word-then-abbrev (p)
              "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
              (interactive "P")
              (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
                (call-interactively 'ispell-word)
                (setq aft (downcase (or (thing-at-point 'word) "")))
                (unless (string= aft bef)
                  (message "\"%s\" now expands to \"%s\" %sally"
                           bef aft (if p "loc" "glob"))
                  (define-abbrev
                    (if p local-abbrev-table global-abbrev-table)
                    bef aft))))

            ;; Tell ispell.el that ’ can be part of a word.
            (setq ispell-local-dictionary-alist
                  `((nil "[[:alpha:]]" "[^[:alpha:]]"
                         ,(rx (not (any alnum space)))
                         nil ("-B") nil utf-8)))

            ;; Don't send ’ to the subprocess.
            (defun mu-replace-apostrophe (args)
              (cons (replace-regexp-in-string
                     "’" "'" (car args))
                    (cdr args)))
            (advice-add #'ispell-send-string :filter-args
                        #'mu-replace-apostrophe)

            ;; Convert ' back to ’ from the subprocess.
            (defun mu-replace-quote (args)
              (if (not (derived-mode-p 'org-mode))
                  args
                (cons (replace-regexp-in-string
                       "'" "’" (car args))
                      (cdr args))))
            (advice-add #'ispell-parse-output :filter-args
                        #'mu-replace-quote)))

(use-package flyspell ; Spell checking on-the-fly
  :bind ("C-c t s" . flyspell-mode)
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    (bind-key "C-c I"
              (lambda()
                (interactive)
                (ispell-change-dictionary "italiano")
                (flyspell-buffer)))

    (bind-key "C-c E"
              (lambda()
                (interactive)
                (ispell-change-dictionary "british")
                (flyspell-buffer)))

    ;; Free C-M-i for completion
    (unbind-key "M-t" flyspell-mode-map))
  :diminish (flyspell-mode . " ⓢ"))

;;; Language tools
(use-package synosaurus ; An extensible thesaurus
  :ensure t
  :bind (("C-c a L s" . synosaurus-lookup)
         ("C-c a L r" . synosaurus-choose-and-replace)))

(use-package langtool ; LanguageTool for Emacs
  :ensure t
  :bind (("C-c a L w" . langtool-check)
         ("C-c a L W" . langtool-check-done)
         ("C-c a L l" . langtool-switch-default-language)
         ("C-c a L m" . langtool-show-message-at-point)
         ("C-c a L c" . langtool-correct-buffer))
  :init (setq langtool-language-tool-jar
              "~/languagetool/languagetool-commandline.jar"
              langtool-default-language "en-GB"
              langtool-java-bin "/usr/bin/java"))

(use-package writegood-mode ; Find common writing problems
  :ensure t
  :bind ("C-c a L g" . writegood-mode)
  :diminish (writegood-mode . " ⓖ"))

(provide 'mu-languages)

;;; mu-languages.el ends here
