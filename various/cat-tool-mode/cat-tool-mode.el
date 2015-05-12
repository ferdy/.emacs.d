;;;; cat-tool-mode.el --- CAT Tool mode

;;; Author: Boccaperta-IT
;;; Keywords: extensions

;; Code:
(defvar cat-tool-mode-hook nil)

(defvar cat-tool-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map ["\C-x\C-g"] 'save-to-glossary)
    map)
  "Keymap for 'cat-tool-mode'.")

(defvar cat-tool-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for 'cat-tool-mode'.")

(defvar cat-tool-font-lock-keywords
  '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
  "Keyword highlighting specification for 'cat-tool-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.trad\\'" . cat-tool-mode))

;;; Indentation
(defun cat-tool-indent-line ()
  "Indent current line of Cat-Tool code."
  (interactive)
  (when (not visual-line-mode)
    (visual-line-mode +1))
  (beginning-of-line)
  (indent-line-to 0))

(define-derived-mode cat-tool-mode fundamental-mode "CAT-TOOL"
  "A major mode for using Emacs as a CAT tool."
  (set (make-local-variable 'font-lock-defaults) '(cat-tool-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'cat-tool-indent-line))

(provide 'cat-tool-mode)
;;;; cat-tool-mode.el ends here
