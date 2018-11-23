;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2018  Manuel Uberti

;;; Commentary:

;; This file stores my configuration for spell-checking and translations
;; utilities.

;;; Code:

;;; Translation
(use-package po-mode                    ; Manage .po files
  :ensure t
  :defer t
  :mode "\\.po\\'"
  :config (setq po-keep-mo-file t))

;;; Spell checking
(use-package ispell                     ; Word correction
  :defer t
  :config (setq ispell-dictionary "en_GB"
                ispell-choices-win-default-height 5
                ispell-program-name (executable-find "hunspell"))

  (unless ispell-program-name
    (warn "No spell checker available. Please install hunspell."))

  (defun mu-ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (progn
                 (backward-word)
                 (and (setq bef (thing-at-point 'word))
                      (not (ispell-word nil 'quiet)))))
        (setq aft (thing-at-point 'word)))
      (when (and aft bef (not (equal aft bef)))
        (setq aft (downcase aft))
        (setq bef (downcase bef))
        (define-abbrev
          (if p local-abbrev-table global-abbrev-table)
          bef aft)
        (write-abbrev-file)
        (message "\"%s\" now expands to \"%s\" %sally"
                 bef aft (if p "loc" "glob")))))

  (bind-key "C-i" #'mu-ispell-word-then-abbrev ctl-x-map))

(use-package flyspell                   ; Spell checking on-the-fly
  :bind (:map flyspell-mode-map
              ("C-M-l" . mu-cycle-ispell-languages))
  :hook ((prog-mode  . flyspell-prog-mode)
         (text-mode  . flyspell-mode)
         (LaTeX-mode . flyspell-mode))
  :init
  (defun mu-current-dictionary-mode-line (language)
    "Return the current dictionary from LANGUAGE for the mode line."
    (interactive)
    (let ((dict (upcase (substring language 0 2))))
      (concat " " dict)))

  (defvar mu-languages-ring nil "Languages ring for Ispell")

  (let ((languages '("it_IT" "en_GB")))
    (setq mu-languages-ring (make-ring (length languages)))
    (dolist (elem languages) (ring-insert mu-languages-ring elem)))

  (defun mu-cycle-ispell-languages ()
    "Cycle ispell languages in `mu-languages-ring'.
Change dictionary and mode line lighter accordingly."
    (interactive)
    (let ((language (ring-ref mu-languages-ring -1)))
      (ring-insert mu-languages-ring language)
      (ispell-change-dictionary language)
      (setq flyspell-mode-line-string
            (mu-current-dictionary-mode-line language))
      (force-mode-line-update)))
  :config (setq flyspell-use-meta-tab nil
                flyspell-issue-welcome-flag nil
                flyspell-issue-message-flag nil
                flyspell-mode-line-string
                (mu-current-dictionary-mode-line ispell-dictionary))

  ;; Free M-t for transpose words
  (unbind-key "M-t" flyspell-mode-map)
  ;; Free C-M-i for completion-at-point
  (unbind-key "C-M-i" flyspell-mode-map)
  ;; Free C-. (see: mu-editing.el)
  (unbind-key "C-." flyspell-mode-map))

(use-package flyspell-correct-ivy       ; Better interface for corrections
  :ensure t
  :demand t
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-at-point)))

;;; Dictionaries and synonyms
(use-package wordnut                    ; Interface to WordNet
  :ensure t
  :bind (("C-c a W" . wordnut-search)
         ("C-c a w" . wordnut-lookup-current-word)))

(use-package define-word                ; Display definitions of words
  :ensure t
  :bind (("C-c a D" . define-word)
         ("C-c a d" . define-word-at-point))
  :config (setq define-word-limit 5))

(use-package powerthesaurus             ; Powerthesaurus integration
  :ensure t
  :bind ("C-c a s" . powerthesaurus-lookup-word-dwim))

(provide 'mu-languages)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; byte-compile-warnings: (not free-vars unresolved)
;; End:

;;; mu-languages.el ends here
