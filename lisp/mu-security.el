;;; mu-security.el --- Part of my Emacs setup -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2015  Manuel Uberti

;; Author: Manuel Uberti <manuel@boccaperta.com>
;; Keywords: convenience

;;; Commentary:

;; This file stores my security configuration.

;;; Code:

;; Verify secure connections
(defun mu-certificate-bundle ()
  "Get the path to a certificate bundle."
  (let* ((lines (or (ignore-errors (process-lines "python3" "-m" "certifi"))
                    (ignore-errors (process-lines "python2" "-m" "certifi"))
                    (ignore-errors (process-lines "python" "-m" "certifi"))))
         (cert-bundle (car lines)))
    (unless (file-exists-p cert-bundle)
      (error "Failed to find a certificate bundle.  Install certifi"))
    cert-bundle))

(setq gnutls-verify-error t)

(if (gnutls-available-p)
    (setq gnutls-trustfiles (mu-certificate-bundle))
  (run-with-idle-timer
   2 nil
   (lambda ()
     (lwarn 'emacs
            :warning
            "GNUTLS is missing!  Certificate validation _not_ configured"))))

(provide 'mu-security)

;;; mu-security.el ends here
