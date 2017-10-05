;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Manuel Uberti

;; Author: Manuel Uberti manuel.uberti@inventati.org
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for languages and translations utilities.

;;; Code:

;;; Translation
(use-package po-mode                    ; Manage .po files
  :ensure t
  :defer t
  :mode "\\.po\\'"
  :config (validate-setq po-keep-mo-file t))

;;; Spell checking
(use-package ispell                     ; Word correction
  :defer t
  :config
  (validate-setq
   ispell-program-name (executable-find "hunspell")
   ispell-dictionary "en_GB"
   ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available.  Plese install hunspell."))

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
  :init
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)

  (defvar mu-languages-ring nil "Languages ring for Ispell")

  (let ((languages '("en_GB" "it_IT")))
    (validate-setq mu-languages-ring (make-ring (length languages)))
    (dolist (elem languages) (ring-insert mu-languages-ring elem)))

  (defun mu-cycle-ispell-languages ()
    (interactive)
    (let ((language (ring-ref mu-languages-ring -1)))
      (ring-insert mu-languages-ring language)
      (ispell-change-dictionary language)))

  (dolist (mode-hook '(text-mode-hook LaTeX-mode-hook))
    (add-hook mode-hook (lambda () (flyspell-mode))))
  :config
  (validate-setq
   flyspell-use-meta-tab nil
   ;; Make Flyspell less chatty
   flyspell-issue-welcome-flag nil
   flyspell-issue-message-flag nil)

  ;; Free M-t for transpose words
  (unbind-key "M-t" flyspell-mode-map)
  ;; Free C-M-i for completion-at-point
  (unbind-key "C-M-i" flyspell-mode-map)
  :diminish flyspell-mode)

(use-package flyspell-correct-ivy       ; Better interface for corrections
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-c $" . flyspell-correct-word-generic)))

(use-package auto-correct               ; Automatically fix past corrections
  :ensure t
  :init (auto-correct-mode)
  :diminish auto-correct-mode)

;;; Dictionaries and synonyms
(use-package wordnut                    ; Interface to WordNet
  :ensure t
  :bind (("C-c a L d" . wordnut-search)
         ("C-c a L D" . wordnut-lookup-current-word)))

(provide 'mu-languages)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-languages.el ends here
