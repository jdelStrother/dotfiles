;;; clipboard.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jonathan del Strother

;; Author: Jonathan del Strother <me@delstrother.com>
;;
;; By default emacs makes a mess of my systemwide clipboard history,
;; with every deletion and `xp` getting exported into it.
;; Instead I use simpleclip so that the killring doesn't alter clipboard history by default,
;; and then remap `y` & `Y` so that those 'intentional' copy operations _are_ exported to the system clipboard.
;; We can't just advise evil-yank because it's used all over the place for 'unintentional' copy operations.
(simpleclip-mode 1)

(evil-define-operator jds-evil-yank-to-system (beg end type register yank-handler)
  "Save the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (evil-yank beg end type register yank-handler)
  (if (not register)
      (simpleclip-set-contents (evil-get-register ?0))
    )
  )
(evil-define-operator jds-evil-yank-line-to-system (beg end type register)
  "Save whole lines into the kill-ring."
  :motion evil-line-or-visual-line
  :move-point nil
  (interactive "<R><x>")
  (evil-yank-line beg end type register)
  (if (not register)
      (simpleclip-set-contents (evil-get-register ?0))
    )
  )
(define-key evil-normal-state-map "y" 'jds-evil-yank-to-system)
(define-key evil-normal-state-map "Y" 'jds-evil-yank-line-to-system)
(define-key evil-motion-state-map "y" 'jds-evil-yank-to-system)
(define-key evil-motion-state-map "Y" 'jds-evil-yank-line-to-system)

(defun jds-copy-to-system-clipboard (&rest _args)
  (simpleclip-set-contents (car kill-ring)))
(advice-add 'browse-at-remote-kill :after 'jds-copy-to-system-clipboard)
(advice-add 'evil-collection-magit-yank-whole-line :after 'jds-copy-to-system-clipboard)
(advice-add 'magit-copy-buffer-revision :after 'jds-copy-to-system-clipboard)


;; emacs-everywhere needs simpleclip disabling during its operations:
(defun jds-disable-simpleclip-around (orig-fun &rest args)
  "Temporarily disable `simpleclip-mode` around the execution of ORIG-FUN."
  (let ((was-enabled (bound-and-true-p simpleclip-mode)))
    ;; Disable simpleclip-mode if it is enabled
    (when was-enabled (simpleclip-mode -1))
    ;; Call the original function
    (unwind-protect (apply orig-fun args)
      ;; Re-enable simpleclip-mode if it was previously enabled
      (when was-enabled (simpleclip-mode 1)))))

(advice-add 'emacs-everywhere-initialise :around #'jds-disable-simpleclip-around)
(advice-add 'emacs-everywhere-finish :around #'jds-disable-simpleclip-around)
(setq emacs-everywhere-clipboard-sleep-delay 0.2)
