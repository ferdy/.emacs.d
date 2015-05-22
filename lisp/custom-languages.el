;;; custom-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

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
(use-package ispell
  :defer t
  :config (progn
            (setq ispell-program-name (executable-find "aspell")
                  ispell-dictionary "italiano"
                  ispell-choices-win-default-height 5)

            (unless ispell-program-name
              (warn "No spell checker available. Install aspell."))

            ;; Ispell with Abbrev for auto-correcting spelling mistakes
            (bind-key "C-i" #'custom/ispell-word-then-abbrev ctl-x-map)

            (defun custom/ispell-word-then-abbrev (p)
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
            (defun custom/replace-apostrophe (args)
              (cons (replace-regexp-in-string
                     "’" "'" (car args))
                    (cdr args)))
            (advice-add #'ispell-send-string :filter-args
                        #'custom/replace-apostrophe)

            ;; Convert ' back to ’ from the subprocess.
            (defun custom/replace-quote (args)
              (if (not (derived-mode-p 'org-mode))
                  args
                (cons (replace-regexp-in-string
                       "'" "’" (car args))
                      (cdr args))))
            (advice-add #'ispell-parse-output :filter-args
                        #'custom/replace-quote)))

(use-package flyspell
  :bind (("C-c s b" . flyspell-buffer)
         ("C-c s r" . flyspell-region))
  :config (progn
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
            (unbind-key "M-t" flyspell-mode-map))
  :diminish flyspell-mode)

(use-package synosaurus ; An extensible thesaurus
  :ensure t
  :bind (("C-c s l" . synosaurus-lookup)
         ("C-c s r" . synosaurus-choose-and-replace)))

(use-package langtool ; Interact with LanguageTool
  :ensure t
  :defer t
  :config (progn
            (setq langtool-language-tool-jar ; Set language tool jar
                  "~/emacs/languagetool-2.8/languagetool-commandline.jar"
                  langtool-java-bin "/usr/bin/java"
                  langtool-mother-tongue "en")))

(use-package voca-builder ; Popup dictionary entries
  :ensure t
  :bind (("C-c s p" . voca-builder/search-popup)
         ("C-c s s" . voca-builder/search))
  :config (setq voca-builder/voca-file "~/org/voca_entries.org"
                ;; Don't record the vocabulary
                voca-builder/record-new-vocabulary nil))

(use-package define-word ; Lookup word definition
  :ensure t
  :bind ("C-c s d" . define-word-at-point))

(provide 'custom-languages)

;;; custom-languages.el ends here
