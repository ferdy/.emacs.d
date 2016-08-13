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
    (warn "No spell checker available.  Plese install hunspell.")))

(use-package flyspell                   ; Spell checking on-the-fly
  :bind (:map flyspell-mode-map
              ("C-M-i" . mu-cycle-ispell-languages))
  :init
  (defvar mu-languages-ring nil "Languages ring for Ispell")

  (let ((languages '("en_GB" "it_IT")))
    (setq mu-languages-ring (make-ring (length languages)))
    (dolist (elem languages) (ring-insert mu-languages-ring elem)))

  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  (defun mu-cycle-ispell-languages ()
    (interactive)
    (let ((language (ring-ref mu-languages-ring -1)))
      (ring-insert mu-languages-ring language)
      (ispell-change-dictionary language)))

  (dolist (mode-hook '(text-mode-hook LaTeX-mode-hook))
    (add-hook mode-hook (lambda () (flyspell-mode))))
  :config
  (setq flyspell-use-meta-tab nil
        ;; Make Flyspell less chatty
        flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)

  ;; Free M-t for transpose words
  (unbind-key "M-t" flyspell-mode-map))

(use-package flyspell-correct-ivy           ; Better interface for corrections
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic)))

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
