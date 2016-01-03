;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for languages and translations utilities.

;;; Code:

;;; Translation
(use-package po-mode                    ; Manage .po files
  :load-path "various"
  :mode "\\.po\\'"
  :no-require t
  :config (setq po-keep-mo-file t))

;;; Spell checking and dictionaries
(use-package ispell                     ; Word correction
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

(use-package flyspell                   ; Spell checking on-the-fly
  :bind ("C-c t s" . flyspell-mode)
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          ;; Make Flyspell less chatty
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    (bind-key "C-c I"
              (lambda ()
                (interactive)
                (ispell-change-dictionary "italiano")
                (flyspell-buffer)))

    (bind-key "C-c E"
              (lambda ()
                (interactive)
                (ispell-change-dictionary "british")
                (flyspell-buffer)))

    ;; Free C-M-i for completion
    (unbind-key "M-t" flyspell-mode-map)))

;;; Language tools
(use-package wordnut                    ; Interface to WordNet
  :ensure t
  :bind (("C-c a L d" . wordnut-search)
         ("C-c a L D" . wordnut-lookup-current-word)))

(use-package synosaurus                 ; An extensible thesaurus
  :ensure t
  :bind (("C-c a L s" . synosaurus-lookup)
         ("C-c a L r" . synosaurus-choose-and-replace)))

(use-package langtool                   ; LanguageTool for Emacs
  :ensure t
  :bind (("C-c a L l w" . langtool-check)
         ("C-c a L l W" . langtool-check-done)
         ("C-c a L l l" . langtool-switch-default-language)
         ("C-c a L l m" . langtool-show-message-at-point)
         ("C-c a L l c" . langtool-correct-buffer))
  :init (setq langtool-language-tool-jar
              "~/languagetool/languagetool-commandline.jar"
              langtool-default-language "en-GB"
              langtool-java-bin "/usr/bin/java"))

(use-package writegood-mode             ; Find common writing problems
  :ensure t
  :bind ("C-c a L g" . writegood-mode)
  :diminish writegood-mode)

;;; Utilities and keybindings
;;;###autoload
(defun mu--wordreference (languages &optional word)
  "Use LANGUAGES to translate WORD or prompted text with WordReference."
  (browse-url
   (concat
    "http://www.wordreference.com/" languages "/"
    (if (stringp word)
        (downcase word)
      (downcase (read-string "WordReference: "))))))

;;;###autoload
(defun mu--wordreference-at-point (languages)
  "Use `mu--wordreference' with LANGUAGES to translate word at point."
  (mu--wordreference languages
                     (substring-no-properties
                      (thing-at-point 'word))))

(defun mu-wordreference-iten ()
  "Use `mu--wordreference' to translate IT>EN."
  (interactive)
  (mu--wordreference "iten"))

(defun mu-wordreference-enit ()
  "Use `mu--wordreference' to translate EN>IT."
  (interactive)
  (mu--wordreference "enit"))

(defun mu-wordreference-iten-at-point ()
  "Use `mu--wordreference-at-point' to translate IT>EN."
  (interactive)
  (mu--wordreference-at-point "iten"))

(defun mu-wordreference-enit-at-point ()
  "Use `mu--wordreference-at-point' to translate EN>IT."
  (interactive)
  (mu--wordreference-at-point "enit"))

(bind-keys
 ("C-c a L t i" . mu-wordreference-iten)
 ("C-c a L t I" . mu-wordreference-iten-at-point)
 ("C-c a L t e" . mu-wordreference-enit)
 ("C-c a L t E" . mu-wordreference-enit-at-point))

(provide 'mu-languages)

;;; mu-languages.el ends here
