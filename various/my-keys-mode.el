;;; my-keys-mode.el ---                              -*- lexical-binding: t; -*-

;;; Commentary:
;; Source: https://emacs.stackexchange.com/a/358/219

;; My minor mode
;; Main use is to have my key bindings have the highest priority

;;; Code:
(defvar my-keys-mode-map (make-sparse-keymap)
  "Keymap while my-keys-mode is active.")

;;;###autoload
(define-minor-mode my-keys-mode
  "A minor mode so that my key settings override annoying major modes."
  nil
  :lighter " my-keys-mode"
  my-keys-mode-map)

;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-mode))
      (let ((mykeys (assq 'my-keys-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

;;;###autoload
(defun turn-on-my-keys-mode ()
  "Turn on my-keys-mode."
  (interactive)
  (my-keys-mode t))

;;;###autoload
(defun turn-off-my-keys-mode ()
  "Turn off my-keys-mode."
  (interactive)
  (my-keys-mode -1))

;;;###autoload
(define-globalized-minor-mode global-my-keys-mode my-keys-mode turn-on-my-keys-mode)

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook 'turn-off-my-keys-mode)

(provide 'my-keys-mode)

;;; my-keys-mode.el ends here
