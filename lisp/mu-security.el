;;; mu-security.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my security configuration.

;;; Code:

;; Verify secure connections
(setq gnutls-verify-error t)

(unless (gnutls-available-p)
  (run-with-idle-timer
   2 nil
   (lambda ()
     (lwarn 'emacs
            :warning
            "GNUTLS is missing!  Certificate validation _not_ configured"))))

(provide 'mu-security)

;;; mu-security.el ends here
