;;; hlds-mode.el --- highlight doublespace after period  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Jonathan del Strother

;; Author: Jonathan del Strother <jdelStrother@gmail.com>

(defface hlds
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "Red1"))
    (t
     :underline t :inherit error))
  "Face used to mark missing sentence separating double spaces."
  :group 'hlds)

(defvar hlds-re "[.!?]\\(  \\)"
  "Regular expression for marking double spaces as sentence separators.")

(defun hlds-clear (start end)
  "Clear hlds fontification in region from START to END."
  (with-silent-modifications
    (cl-loop for int being the intervals property 'face from start to end
         if (eq (get-text-property (car int) 'face) 'hlds)
         do (remove-text-properties (car int) (cdr int) '(face hdls)))))

(defun hlds-jitlock-handler (start end)
  "Mark double spaces in region from START to END."
  (save-excursion
   (with-silent-modifications
     (hlds-clear start end)
     (goto-char (max (- start 2) 1))
     (when (< end (point-max))
       (cl-incf end))
     (while (re-search-forward hlds-re end t)
       (put-text-property (match-beginning 1) (match-end 1) 'face 'hlds)))))

(defvar-local hlds-font-lock-keywords nil
  "Additional `font-lock-keywors' entries for hdls-mode.")

(define-minor-mode hlds-mode
  "Highlight double-spaces that should separate sentences."
  :lighter " ds"
  (if hlds-mode
    (if font-lock-mode
      (progn
        (font-lock-add-keywords
         nil
         (setq hlds-font-lock-keywords `((,hlds-re 1 'hlds t))))
        (save-restriction
          (widen)
          (font-lock-flush)
          (font-lock-ensure)))
    (unless jit-lock-mode (jit-lock-mode t))
    (jit-lock-register #'hlds-jitlock-handler)
    (jit-lock-fontify-now (point-min) (point-max)))
    (when font-lock-mode
      (font-lock-remove-keywords nil hlds-font-lock-keywords)
      (save-restriction
    (widen)
    (font-lock-flush (point-min) (point-max))))
    (when jit-lock-mode
      (save-restriction
    (widen)
    (hlds-clear (point-min) (point-max)))
      (jit-lock-unregister #'hlds-jitlock-handler))))

(define-globalized-minor-mode global-hlds-mode hlds-mode
  (lambda () (hlds-mode 1)))

;; (global-hlds-mode 1)
