;;; custom-smartparens.el --- Part of my Emacs Setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores pairs balancing configuration

;;; Code:
(use-package paredit ; Balanced sexp editing
  :ensure t
  :defer t
  :init (dolist (hook '(eval-expression-minibuffer-setup-hook
                        emacs-lisp-mode-hook
                        inferior-emacs-lisp-mode-hook
                        clojure-mode-hook))
          (add-hook hook #'paredit-mode))
  :config (progn
            ;; Free M-s. There are some useful bindings in that prefix map.
            (unbind-key "M-s" paredit-mode-map)
            (bind-key "M-S-<up>" #'paredit-splice-sexp paredit-mode-map)

            ;; Extreme barfage and slurpage
            (defun paredit-barf-all-the-way-backward ()
              (interactive)
              (paredit-split-sexp)
              (paredit-backward-down)
              (paredit-splice-sexp))

            (defun paredit-barf-all-the-way-forward ()
              (interactive)
              (paredit-split-sexp)
              (paredit-forward-down)
              (paredit-splice-sexp)
              (if (eolp) (delete-horizontal-space)))

            (defun paredit-slurp-all-the-way-backward ()
              (interactive)
              (catch 'done
                (while (not (bobp))
                  (save-excursion
                    (paredit-backward-up)
                    (if (eq (char-before) ?\()
                        (throw 'done t)))
                  (paredit-backward-slurp-sexp))))

            (defun paredit-slurp-all-the-way-forward ()
              (interactive)
              (catch 'done
                (while (not (eobp))
                  (save-excursion
                    (paredit-forward-up)
                    (if (eq (char-after) ?\))
                        (throw 'done t)))
                  (paredit-forward-slurp-sexp))))

            (nconc paredit-commands
                   '("Extreme Barfage & Slurpage"
                     (("C-M-)" "M-N")
                      paredit-slurp-all-the-way-forward
                      ("(foo (bar |baz) quux zot)"
                       "(foo (bar |baz quux zot))")
                      ("(a b ((c| d)) e f)"
                       "(a b ((c| d)) e f)"))
                     (("C-M-}" "M-F")
                      paredit-barf-all-the-way-forward
                      ("(foo (bar |baz quux) zot)"
                       "(foo (bar|) baz quux zot)"))
                     (("C-M-(" "M-P")
                      paredit-slurp-all-the-way-backward
                      ("(foo bar (baz| quux) zot)"
                       "((foo bar baz| quux) zot)")
                      ("(a b ((c| d)) e f)"
                       "(a b ((c| d)) e f)"))
                     (("C-M-{" "M-B")
                      paredit-barf-all-the-way-backward
                      ("(foo (bar baz |quux) zot)"
                       "(foo bar baz (|quux) zot)"))))

            (paredit-define-keys)
            (paredit-annotate-mode-with-examples)
            (paredit-annotate-functions-with-examples))
  :diminish paredit-mode)

(use-package redshank ; Lisp editing extension
  :ensure t
  :defer t
  :init (progn
          (setq redshank-prefix-key "C-c C-r") ; Change default prefix
          (dolist (hook '(eval-expression-minibuffer-setup-hook
                          emacs-lisp-mode-hook
                          inferior-emacs-lisp-mode-hook
                          clojure-mode-hook))
            (add-hook hook #'redshank-mode)))
  :diminish redshank-mode)

(add-hook 'after-save-hook ; Look for unbalanced parens when saving
          'check-parens nil t)

(provide 'custom-pairs)

;;; custom-pairs.el ends here
