;;; custom-editing.el --- Part of my Emacs setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores editing customizations.

;;; Code:

(use-package zop-to-char ; Better zap-to-char
  :ensure t
  :bind (("M-z" . zop-to-char)
         ("M-Z" . zop-up-to-char)))

(use-package whitespace-cleanup-mode ; Cleanup whitespace in buffers
  :ensure t
  :bind (("C-c t c" . whitespace-cleanup-mode)
         ("C-c x w" . whitespace-cleanup))
  :init (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
          (add-hook hook #'whitespace-cleanup-mode))
  :diminish whitespace-cleanup-mode)

(use-package shrink-whitespace ; Better whitespace removal
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package undo-tree ; Show buffer changes as a tree
  :ensure t
  :init (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package delsel ; Delete the selection instead of insert
  :defer t
  :init (delete-selection-mode))

(use-package fix-word ; Convenient word transformation
  :ensure t
  :bind (("M-u" . fix-word-upcase)
         ("M-l" . fix-word-downcase)
         ("M-c" . fix-word-capitalize)))

(use-package expand-region ; Increase the selected region by semantic units
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package easy-kill ; Better kill text
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
         ("C-c e k" . easy-mark)))

(use-package adaptive-wrap ; Better line wrap
  :ensure t
  :defer t
  :init (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode))

(use-package aggressive-fill-paragraph ; Automatically fill paragrah
  :ensure t
  :defer t
  :config (progn
            (add-hook 'org-mode-hook #'aggressive-fill-paragraph-mode)
            (add-hook 'TeX-mode-hook #'aggressive-fill-paragraph-mode)))

(use-package visual-fill-column ; Wrap at fill column
  :ensure t
  :init (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))

(use-package aggressive-indent ; Automatically indent code
  :ensure t
  :init (global-aggressive-indent-mode 1)
  :config (add-to-list 'aggressive-indent-excluded-modes
                       'cider-repl-mode))

(use-package align ; Align text in buffers
  :bind ("C-c x a a" . align))

(use-package ediff-wind ; Better ediff behavior
  :defer 5
  :config (setq ediff-window-setup-function #'ediff-setup-windows-plain
                ediff-split-window-function #'split-window-horizontally))

(use-package multiple-cursors ; Easily place multiple cursors in a buffer
  :ensure t
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
         ("C-c o a"     . mc/vertical-align)
         ("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o n"     . mc/mark-next-like-this)
         ("C-c o p"     . mc/mark-previous-like-this)
         ("C-c o r"     . vr/mc-mark)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o C-s"   . mc/mark-all-in-region))
  :config (setq mc/mode-line
                ;; Simplify the MC mode line indicator
                '(:propertize (:eval (concat " " (number-to-string
                                                  (mc/num-cursors))))
                              face font-lock-warning-face)))

(use-package multifiles ; Edit multiple files at once
  :ensure t
  :bind ("C-!" . mf/mirror-region-in-multifile))

(use-package saveplace ; Save point position in files
  :init (save-place-mode 1))

(use-package autorevert ; Auto-revert buffers of changed files
  :init (global-auto-revert-mode)
  :config (setq auto-revert-verbose nil
                ;; Revert Dired buffers, too
                global-auto-revert-non-file-buffers t))

(use-package transpose-mark ; Transpose data by leaving an Emacs mark
  :ensure t                 ; on the line you want to transpose.
  :bind ("C-c t m" . transpose-mark))

(use-package auto-insert ; Automatic insertion into new files
  :defer t
  :bind ("C-c i a" . auto-insert))

(use-package copyright ; Deal with copyright notices
  :defer t
  :bind ("C-c i c" . copyright-update)
  ;; Update copyright when visiting files
  :init (add-hook 'find-file-hook #'copyright-update)
  ;; Use ranges to denote consecutive years
  :config (setq copyright-year-ranges t
                copyright-names-regexp (regexp-quote user-full-name)))

(use-package easy-escape ; Improve escape backslashes readability
  :ensure t
  :config (add-hook 'prog-mode-hook 'easy-escape-minor-mode)
  :diminish easy-escape-minor-mode)

(use-package dubcaps-mode ; DOuble CApitals to Single Capitals
  :load-path "various"
  :init (add-hook 'text-mode-hook #'dubcaps-mode)
  :diminish dubcaps-mode)

(use-package typo ; Automatically use typographic quotes
  :ensure t
  :init (progn
          (typo-global-mode)

          (dolist (hook '(markdown-mode-hook
                          rst-mode-hook))
            (add-hook hook 'typo-mode)))
  :diminish typo-mode)

(use-package goto-addr ; Make links clickable
  :defer t
  :init (progn (add-hook 'prog-mode-hook #'goto-address-prog-mode)
               (add-hook 'text-mode-hook #'goto-address-mode)
               (add-hook 'org-mode-hook (lambda ()
                                          (goto-address-mode -1)))))

(use-package string-edit ; Edit strings in a separate buffer
  :ensure t
  :commands string-edit-at-point)

(use-package writeroom-mode ; Distraction-free interface
  :ensure t
  :bind ("C-c t r" . writeroom-mode))

(use-package indent ; Built-in indentation
  :bind ("C-c x i" . indent-region))

(setq next-line-add-newlines t) ; C-n adds new line when at the end of a line

;;; Utilities and keybindings
;; Kill entire line with prefix argument
(defmacro bol-with-prefix (function)
  "Define a new function which calls FUNCTION.
Except it moves to beginning of line before calling FUNCTION when
called with a prefix argument.  The FUNCTION still receives the
prefix argument."
  (let ((name (intern (format "custom/%s-BOL" function))))
    `(progn
       (defun ,name (p)
         ,(format
           "Call `%s', but move to BOL when called with a prefix argument."
           function)
         (interactive "P")
         (when p
           (forward-line 0))
         (call-interactively ',function))
       ',name)))

(bind-key [remap sp-kill-hybrid-sexp] (bol-with-prefix sp-kill-hybrid-sexp))
(bind-key [remap org-kill-line] (bol-with-prefix org-kill-line))
(bind-key [remap kill-line] (bol-with-prefix kill-line))
(bind-key "C-k" (bol-with-prefix kill-visual-line))

(defun just-one-space-in-region (beg end)
  "Replace all whitespace in the region from BEG to END with single spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" 'duplicate-line) ; Duplicate line at point

(defun flush-kill-lines (regex)
  "Flush lines matching REGEX and append to kill ring.  Restrict to \
region if active."
  (interactive "sFlush kill regex: ")
  (save-excursion
    (save-restriction
      (when (use-region-p)
        (narrow-to-region (point) (mark))
        (goto-char 0))
      (while (search-forward-regexp regex nil t)
        (move-beginning-of-line nil)
        (kill-whole-line)))))

(defun unfill-paragraph (&optional region)
  "Turn a multi-line paragraph into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(bind-key "M-Q" #'unfill-paragraph) ; The opposite of fill-paragraph

(defun custom/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let ((complete-regexp (if after
                             (concat regexp "\\([ \t]*\\)")
                           (concat "\\([ \t]*\\)" regexp)))
        (group (if justify-right -1 1)))
    (align-regexp start end complete-regexp group 1 t)))

(defun custom/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)."
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro custom/create-align-repeat-x
    (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "custom/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (custom/align-repeat start end ,regexp ,justify-right after)))))

(custom/create-align-repeat-x "comma" "," nil t)
(custom/create-align-repeat-x "semicolon" ";" nil t)
(custom/create-align-repeat-x "colon" ":" nil t)
(custom/create-align-repeat-x "equal" "=")
(custom/create-align-repeat-x "math-oper" "[+\\-*/]")
(custom/create-align-repeat-x "ampersand" "&")
(custom/create-align-repeat-x "bar" "|")
(custom/create-align-repeat-x "left-paren" "(")
(custom/create-align-repeat-x "right-paren" ")" t)

(bind-key "C-c x a r" #'custom/align-repeat)
(bind-key "C-c x a m" #'custom/align-repeat-math-oper)
(bind-key "C-c x a ." #'custom/align-repeat-decimal)
(bind-key "C-c x a ," #'custom/align-repeat-comma)
(bind-key "C-c x a ;" #'custom/align-repeat-semicolon)
(bind-key "C-c x a :" #'custom/align-repeat-colon)
(bind-key "C-c x a =" #'custom/align-repeat-equal)
(bind-key "C-c x a &" #'custom/align-repeat-ampersand)
(bind-key "C-c x a |" #'custom/align-repeat-bar)
(bind-key "C-c x a (" #'custom/align-repeat-left-paren)
(bind-key "C-c x a )" #'custom/align-repeat-right-paren)

(provide 'custom-editing)

;;; custom-editing.el ends here
