;;; mu-modeline.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Manuel Uberti

;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; Keywords: convenience

;;; Commentary:

;; This file stores my modeline configuration.
;; It needs the fonts from https://github.com/domtronn/all-the-icons.el

;;; Code:

(line-number-mode)
(column-number-mode)

(use-package s)
(use-package all-the-icons
  :ensure t)

(defface zerodark-modeline-buffer
  '((((class color) (min-colors 89)) :background "#61afef"))
  "Face used to display the buffer informations.")

(defface zerodark-modeline-git
  '((((class color) (min-colors 89)) :background "#98be65"))
  "Face used to display git informations.")

(defvar zerodark-modeline-position "%l:%c %p "
  "Mode line construct for displaying the position in the buffer.")

(defvar zerodark-modeline-modified
  '(:eval (if (buffer-modified-p (current-buffer))
              (all-the-icons-faicon "floppy-o" :height 1 :v-adjust 0)
            (all-the-icons-faicon "check" :height 1 :v-adjust 0))))

(defvar zerodark-modeline-ro
  '(:eval (if buffer-read-only (propertize "RO " 'face 'bold) "")))

(defvar zerodark-buffer-coding
  '(:eval (unless (eq buffer-file-coding-system
                      (default-value 'buffer-file-coding-system))
            mode-line-mule-info)))

(defvar zerodark-modeline-vc
  '(vc-mode ("   "
             (:eval (all-the-icons-faicon "code-fork" :height 1 :v-adjust 0))
             (:eval (s-truncate 25 vc-mode)))))

(setq-default mode-line-format
              `("%e"
                mode-line-front-space
                ,zerodark-modeline-ro
                ,zerodark-buffer-coding
                mode-line-frame-identification " "
                " "
                ,zerodark-modeline-modified
                " "
                mode-line-buffer-identification
                ,zerodark-modeline-vc
                "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
                "  " ,zerodark-modeline-position
                ))

(let ((class '((class color) (min-colors 89)))
      (mode-line-bg "#6f337e")
      (mode-line-fg "#ccd4e3")
      (mode-line-inactive-bg "#21252b")
      (mode-line-inactive-fg "#687080"))
  (custom-theme-set-faces
   'zerodark
   `(mode-line
     ((,class (:background ,mode-line-bg
                           :height 0.9
                           :foreground ,mode-line-fg
                           :box ,(list :line-width 6
                                       :color mode-line-bg)))))
   `(mode-line-inactive
     ((,class (:background ,mode-line-inactive-bg
                           :height 0.9
                           :foreground ,mode-line-inactive-fg
                           :box ,(list :line-width 6
                                       :color mode-line-inactive-bg)))))))

(provide 'mu-modeline)

;;; mu-modeline.el ends here
