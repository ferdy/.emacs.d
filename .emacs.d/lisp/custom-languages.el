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
  :init (setq auto-mode-alist
              (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist)))

;;; Spell checking and dictionaries
;; Requires: aspell, aspell-in, aspell-en
(use-package ispell
  :defer t
  :config
  (progn
    (setq ispell-program-name (executable-find "aspell")
          ispell-dictionary "italiano"
          ispell-choices-win-default-height 5)

    (unless ispell-program-name
      (warn "No spell checker available. Install aspell."))))

(use-package flyspell
  :defer t
  :config
  (progn
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
    (define-key flyspell-mode-map "\M-\t" nil)))

;; Requires: wordnet
(use-package synosaurus ; An extensible thesaurus
  :ensure t
  :defer t
  :bind (("C-c s l" . synosaurus-lookup)
         ("C-c s r" . synosaurus-choose-and-replace)))

;; Requires: languagetool (https://www.languagetool.org/)
(use-package langtool
  :ensure t
  :defer t
  :config
  (progn
    (setq langtool-language-tool-jar ; Set language tool jar
          "~/emacs/languagetool-2.8/languagetool-commandline.jar"
          langtool-java-bin "/usr/bin/java"
          langtool-mother-tongue "en")))

(use-package voca-builder ; Popup dictionary entries
  :ensure t
  :defer t
  :bind (("C-c v s" . voca-builder/search-popup))
  :config
  (setq voca-builder/voca-file "~/org/voca_entries.org"))

(provide 'custom-languages)

;;; custom-languages.el ends here
