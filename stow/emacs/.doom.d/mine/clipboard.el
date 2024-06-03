;;; clipboard.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jonathan del Strother

;; Author: Jonathan del Strother <me@delstrother.com>
;;
;; By default emacs+evil make a mess of my systemwide clipboard history,
;; with every deletion and `xp` getting exported into it.
;; Instead I disable emacs' clipboard integration so that the killring doesn't alter clipboard history by default,
;; and then remap `y` & `Y` so that those 'intentional' copy operations _are_ exported to the system clipboard.

(setq select-enable-clipboard nil)

(defadvice! jds-force-enable-clipboard (orig-fun &rest args)
  "enables clipboard sync for commands that benefit from it.
E.g. for commands that copy particularly useful text."
  :around '(+default/yank-buffer-path
            +default/yank-buffer-contents
            +default/yank-buffer-path-relative-to-project
            browse-at-remote-kill
            evil-collection-magit-yank-whole-line
            magit-copy-buffer-revision
            emacs-everywhere-initialise
            emacs-everywhere-finish)
  (let ((select-enable-clipboard t))
    (apply orig-fun args)))

;; We can't just advise evil-yank because it's used all over the place for 'unintentional' copy operations.
;; Replace the `y` & `Y` key commands instead.
(evil-define-operator jds-evil-yank-to-system (beg end type register yank-handler)
  "Save the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((select-enable-clipboard t))
    (evil-yank beg end type register yank-handler)))

(evil-define-operator jds-evil-yank-line-to-system (beg end type register)
  "Save whole lines into the kill-ring."
  :motion evil-line-or-visual-line
  :move-point nil
  (interactive "<R><x>")
  (let ((select-enable-clipboard t))
    (evil-yank-line beg end type register)))

(define-key evil-normal-state-map "y" 'jds-evil-yank-to-system)
(define-key evil-normal-state-map "Y" 'jds-evil-yank-line-to-system)
(define-key evil-motion-state-map "y" 'jds-evil-yank-to-system)
(define-key evil-motion-state-map "Y" 'jds-evil-yank-line-to-system)
