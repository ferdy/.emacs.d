;;; mu-smartparens.el --- Part of my Emacs Setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my pairs balancing configuration.

;;; Code:

(use-package smartparens                ; Parenthesis editing and balancing
  :ensure t
  :bind (("C-c 0" . mu-smartparens/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :init
  ;; Hydra for Smartparens
  (defhydra mu-smartparens (:hint nil)
    "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _<left>_:    slurp forward   _R_:      splice
_b_: backward    _<right>_:   barf forward    _r_:      raise
_u_: backward ↑  _C-<left>_:  slurp backward  _<up>_:   raise backward
_d_: forward ↓   _C-<right>_: barf backward   _<down>_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (a) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (a) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (a) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (a) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

  (smartparens-global-mode)
  (show-smartparens-global-mode)

  (dolist (hook '(inferior-emacs-lisp-mode-hook
                  emacs-lisp-mode-hook
                  clojure-mode-hook))
    (add-hook hook #'smartparens-strict-mode))
  :config
  (require 'smartparens-config)

  (setq sp-autoskip-closing-pair 'always
        ;; Don't kill entire symbol on C-k
        sp-hybrid-kill-entire-symbol nil))

(add-hook 'after-save-hook              ; Look for unbalanced parens when saving
          'check-parens nil t)

(provide 'mu-pairs)

;;; mu-pairs.el ends here
