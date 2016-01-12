;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for languages and translations utilities.

;;; Code:

;;; Translation
(use-package po-mode                    ; Manage .po files
  :load-path "extra"
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
              (warn "No spell checker available. Install aspell."))))

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

(defun mu-translate-iten ()
  "Use `mu--wordreference' to translate IT>EN."
  (interactive)
  (mu--wordreference "iten"))

(defun mu-translate-enit ()
  "Use `mu--wordreference' to translate EN>IT."
  (interactive)
  (mu--wordreference "enit"))

(defun mu-translate-iten-at-point ()
  "Use `mu--wordreference-at-point' to translate IT>EN."
  (interactive)
  (mu--wordreference-at-point "iten"))

(defun mu-translate-enit-at-point ()
  "Use `mu--wordreference-at-point' to translate EN>IT."
  (interactive)
  (mu--wordreference-at-point "enit"))

(bind-keys
 ("C-c a L t i" . mu-translate-iten)
 ("C-c a L t I" . mu-translate-iten-at-point)
 ("C-c a L t e" . mu-translate-enit)
 ("C-c a L t E" . mu-translate-enit-at-point))

(provide 'mu-languages)

;;; mu-languages.el ends here
