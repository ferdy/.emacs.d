;;; custom-project.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores the configuration for project management utilities.

;;; Code:

;;; Project Management
(use-package springboard ; Temporarily change default-directory for one command
  :ensure t
  :bind ("C-c p s" . springboard)
  :init (setq springboard-directories
              '("/home/manuel/emacs/emacs/"
                "/home/manuel/githubs/manuel-uberti/emacs/")))

(provide 'custom-project)

;;; custom-project.el ends here
