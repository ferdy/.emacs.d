;;; custom-smartparens.el --- Part of my Emacs Setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores pairs balancing configuration

;;; Code:
(use-package smartparens ; Parenthesis editing and balancing
  :ensure t
  :init (progn
          (smartparens-global-mode)
          (show-smartparens-global-mode)

          (dolist (hook '(inferior-emacs-lisp-mode-hook
                          emacs-lisp-mode-hook
                          clojure-mode-hook))
            (add-hook hook #'smartparens-strict-mode)))
  :config (progn
            (setq sp-autoskip-closing-pair 'always
                  ;; Don't kill entire symbol on C-k
                  sp-hybrid-kill-entire-symbol nil)

            (use-package smartparens-config
              :config (progn
                        (sp-local-pair '(emacs-lisp-mode
                                         lisp-interaction-mode
                                         inferior-emacs-lisp-mode
                                         clojure-mode)
                                       "(" nil :bind "M-(")

                        (bind-keys
                         :map smartparens-mode-map
                         ;; Movement and navigation
                         ("C-M-f"       . sp-forward-sexp)
                         ("C-M-b"       . sp-backward-sexp)
                         ("C-M-u"       . sp-backward-up-sexp)
                         ("C-M-d"       . sp-down-sexp)
                         ("C-M-p"       . sp-backward-down-sexp)
                         ("C-M-n"       . sp-up-sexp)
                         ;; Deleting and killing
                         ("C-M-k"       . sp-kill-sexp)
                         ("C-M-w"       . sp-copy-sexp)
                         ;; Depth changing
                         ("M-S-<up>"    . sp-splice-sexp)
                         ("M-<up>"      . sp-splice-sexp-killing-backward)
                         ("M-<down>"    . sp-splice-sexp-killing-forward)
                         ("M-C-<up>"    . sp-splice-sexp-killing-around)
                         ("M-?"         . sp-convolute-sexp)
                         ;; Barfage & Slurpage
                         ("C-)"         . sp-forward-slurp-sexp)
                         ("C-<right>"   . sp-forward-slurp-sexp)
                         ("C-}"         . sp-forward-barf-sexp)
                         ("C-<left>"    . sp-forward-barf-sexp)
                         ("C-("         . sp-backward-slurp-sexp)
                         ("C-M-<left>"  . sp-backward-slurp-sexp)
                         ("C-{"         . sp-backward-barf-sexp)
                         ("C-M-<right>" . sp-backward-barf-sexp)
                         ;; Miscellaneous commands
                         ("M-S"         . sp-split-sexp)
                         ("M-J"         . sp-join-sexp)
                         ("C-M-t"       . sp-transpose-sexp))

                        (bind-key "M-q" #'sp-indent-defun
                                  smartparens-strict-mode-map)))))

(use-package paredit ; Balanced sexp editing
  :ensure t
  :defer t
  :disabled t
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
