;;; mu-security.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my security configuration.

;;; Code:

;; Verify secure connections
(if (gnutls-available-p)
    (let ((trustfile
           (replace-regexp-in-string
            "\\\\" "/"
            (replace-regexp-in-string
             "\n" ""
             (shell-command-to-string "python -m certifi")))))
      (setq tls-program
            (list
             (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                     "" trustfile)))
      (setq gnutls-verify-error t)
      (setq gnutls-trustfiles (list trustfile)))
  (run-with-idle-timer
   2 nil
   (lambda ()
     (lwarn 'emacs
            :warning
            "GNUTLS is missing!  Certificate validation _not_ configured"))))

(provide 'mu-security)

;;; mu-security.el ends here
