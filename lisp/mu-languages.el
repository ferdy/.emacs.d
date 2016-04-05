;;; mu-languages.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my configuration for languages and translations utilities.

;;; Code:

;;; Translation
(use-package po-mode                    ; Manage .po files
  :load-path "extra"
  :mode "\\.po\\'"
  :config (setq po-keep-mo-file t))

;;; Spell checking and dictionaries
(use-package ispell                     ; Word correction
  :defer t
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-dictionary "british"
        ispell-choices-win-default-height 5)

  (unless ispell-program-name
    (warn "No spell checker available. Install aspell.")))

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
              (ispell-change-dictionary "italiano")
              (flyspell-buffer)))

  (bind-key "C-c E"
            (lambda ()
              (interactive)
              (ispell-change-dictionary "british")
              (flyspell-buffer)))

  ;; Free C-M-i for completion
  (unbind-key "M-t" flyspell-mode-map)

  ;; Use Ivy for flyspell candidates
  (unbind-key "C-c $" flyspell-mode-map)
  (bind-key "C-c $" #'mu-flyspell-correct flyspell-mode-map))

;;; Dictionaries and synonyms
(use-package wordnut                    ; Interface to WordNet
  :ensure t
  :bind (("C-c a L d" . wordnut-search)
         ("C-c a L D" . wordnut-lookup-current-word)))

(use-package synosaurus                 ; An extensible thesaurus
  :ensure t
  :bind (("C-c a L s" . synosaurus-lookup)
         ("C-c a L r" . synosaurus-choose-and-replace)))

;;; Grammar and style
(use-package langtool                   ; LanguageTool for Emacs
  :ensure t
  :bind (("C-c a L l w" . langtool-check)
         ("C-c a L l W" . langtool-check-done)
         ("C-c a L l l" . langtool-switch-default-language)
         ("C-c a L l m" . langtool-show-message-at-point)
         ("C-c a L l c" . langtool-correct-buffer))
  :init
  (setq langtool-language-tool-jar
        "~/languagetool/languagetool-commandline.jar"
        langtool-default-language "en-GB"
        langtool-java-bin "/usr/bin/java"))

(use-package writegood-mode             ; Find common writing problems
  :ensure t
  :bind ("C-c a L g" . writegood-mode))

(use-package clear-text                 ; Force the use of clearer text
  :ensure t
  :bind ("C-c a L c" . clear-text-mode))

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

;;;###autoload
(defun mu-flyspell-correct ()
  "Use Ivy for Flyspell correction.
Adapted from `flyspell-correct-word-before-point'."
  (interactive)
  ;; Use the correct dictionary
  (flyspell-accept-buffer-local-defs)
  (let ((cursor-location (point))
        (word (flyspell-get-word))
        (opoint (point)))
    (if (consp word)
        (let ((start (car (cdr word)))
              (end (car (cdr (cdr word))))
              (word (car word))
              poss ispell-filter)
          ;; Check spelling of the word
          (ispell-send-string "%\n")	; Put in verbose mode
          (ispell-send-string (concat "^" word "\n"))
          ;; Wait until Ispell has processed word
          (while (progn
                   (accept-process-output ispell-process)
                   (not (string= "" (car ispell-filter)))))
          ;; Remove leading empty element
          (setq ispell-filter (cdr ispell-filter))
          ;; Ispell process should return something after word is sent;
          ;; tag word as valid (i.e., skip) otherwise.
          (or ispell-filter
              (setq ispell-filter '(*)))
          (when (consp ispell-filter)
            (setq poss (ispell-parse-output (car ispell-filter))))
          (cond
           ;; Don't correct word
           ((or (eq poss t) (stringp poss)) t)
           ;; Ispell error
           ((null poss) (error "Ispell: error in Ispell process"))
           (t
            ;; The word is incorrect, we have to propose a replacement
            (let ((res (ivy-read "Correction: " (third poss) :preselect word)))
              (cond ((stringp res)
                     (flyspell-do-correct
                      res poss word cursor-location start end opoint))
                    (t
                     (let ((cmd (car res))
                           (wrd (cdr res)))
                       (if (string= wrd word)
                           (flyspell-do-correct
                            cmd poss wrd cursor-location start end opoint)
                         (progn
                           (flyspell-do-correct
                            cmd poss wrd cursor-location start end opoint)
                           (flyspell-do-correct
                            wrd poss word cursor-location start end opoint))))))
              )))
          (ispell-pdict-save t)))))

(provide 'mu-languages)

;;; mu-languages.el ends here
