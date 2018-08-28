;;; mu-slow-keys.el --- Slow keys mode to avoid RSI -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Manuel Uberti

;; Author: Manuel Uberti <manuel.uberti@inventati.org>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides a minor mode that forces you to type slowly.  Chris Done
;; is the original author, I just grabbed his code and tweak it a bit.

;;; Code:

(define-minor-mode slow-keys-mode "Type slowly to avoid RSI."
  :lighter " Slow"
  :global t
  (if (bound-and-true-p slow-keys-mode)
      (add-hook 'post-command-hook 'slow-keys-do)
    (remove-hook 'post-command-hook 'slow-keys-do)))

(defvar slow-keys-repeat 0)
(defvar slow-keys-last-press 0)
(defvar slow-keys-sleep-for 0.5)
(defvar slow-keys-min-delay 0.1)

(defun slow-keys-slow-down (msg)
  "Display warning MSG and sleep before let typing begin again."
  (message "%s" (propertize msg 'face 'compilation-error))
  (redisplay)
  (sleep-for slow-keys-sleep-for))

(defun slow-keys-typing-cmd (cmd)
  "Check whether CMD is `self-insert-command' or `org-self-insert-command'."
  (or (eq cmd 'self-insert-command)
      (eq cmd 'org-self-insert-command)))

(defun slow-keys-ignore-cmd (cmd)
  "Check whether CMD should be ignored by slow typing check."
  (eq cmd 'mwheel-scroll))

(defun slow-keys-do ()
  "Check whether typing or running a command is done slowly enough."
  (unless (or executing-kbd-macro
              (slow-keys-ignore-cmd this-command))
    (setq slow-keys-repeat
          (if (eq last-command this-command)
              (1+ slow-keys-repeat)
            0))
    (when (and (> slow-keys-repeat 3)
               (not (slow-keys-typing-cmd this-command)))
      (slow-keys-slow-down
       (format "Use repetition numbers or more high-level commands: %S"
               this-command)))
    (let ((now (float-time)))
      (cond
       ((and (slow-keys-typing-cmd this-command)
             (slow-keys-typing-cmd last-command)
             (< (- now slow-keys-last-press) 0.1))
        (slow-keys-slow-down "Slow down typing!"))
       ((and (not (slow-keys-typing-cmd this-command))
             (not (slow-keys-typing-cmd last-command))
             (< (- now slow-keys-last-press) slow-keys-min-delay))
        (slow-keys-slow-down "Slow down command running!")))
      (setq slow-keys-last-press now))))

(provide 'mu-slow-keys)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; mu-slow-keys.el ends here
