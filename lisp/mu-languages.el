;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for languages and translations utilities.

;;; Code:

;;; Translation
(use-package po-mode                    ; Manage .po files
  :load-path "extra"
  :defer t
  :mode "\\.po\\'"
  :config (setq po-keep-mo-file t))

;;; Spell checking and dictionaries
(use-package ispell                     ; Word correction
  :defer t
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_GB"
        ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available. Install hunspell.")))

(use-package flyspell                   ; Spell checking on-the-fly
  :bind ("C-c t s" . flyspell-mode)
  :config
  (setq flyspell-use-meta-tab nil
        ;; Make Flyspell less chatty
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)

  (bind-key "C-c I"
            (lambda ()
              (interactive)
              (ispell-change-dictionary "it_IT")
              (flyspell-buffer)))

  (bind-key "C-c E"
            (lambda ()
              (interactive)
              (ispell-change-dictionary "en_GB")
              (flyspell-buffer)))

  ;; Free M-t for transpose words
  (unbind-key "M-t" flyspell-mode-map))

(use-package flyspell-correct           ; Better interface for corrections
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic))
  :config (setq flyspell-correct-interface 'flyspell-correct-ivy))

;;; Dictionaries and synonyms
(use-package wordnut                    ; Interface to WordNet
  :ensure t
  :bind (("C-c a L d" . wordnut-search)
         ("C-c a L D" . wordnut-lookup-current-word)))

;;; Grammar and style
(use-package writegood-mode             ; Find common writing problems
  :ensure t
  :bind ("C-c a L g" . writegood-mode))

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
