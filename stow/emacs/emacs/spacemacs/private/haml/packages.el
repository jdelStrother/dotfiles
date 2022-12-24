;;; packages.el --- haml layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Jonathan del Strother <jon@jons-old-mbp.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `haml-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `haml/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `haml/pre-init-PACKAGE' and/or
;;   `haml/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst haml-packages
  '(haml-mode
    flycheck))

(defun haml/init-haml-mode ()
  (use-package haml-mode
    :defer t
    ))

(defun haml/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (flycheck-define-checker haml-lint
        "A haml syntax checker using the haml-lint tool."
        :command ("bundle"
                  "exec"
                  "haml-lint"
                  source-inplace)
        :working-directory flycheck-ruby--find-project-root
        :error-patterns
        ((info line-start (file-name) ":" line " [C] " (message) line-end)
        (warning line-start (file-name) ":" line " [W] " (message) line-end)
        (error line-start (file-name) ":" line " [" (or "E" "F") "] " (message) line-end))
        :modes (haml-mode))
      (add-to-list 'flycheck-checkers 'haml-lint)
      (flycheck-add-next-checker 'haml '(warning . haml-lint))
      (spacemacs/enable-flycheck 'haml-mode)

      (add-to-list 'compilation-error-regexp-alist-alist
                   '(haml-lint
                     "^\\([^:]+\\):\\([0-9]+\\) \\[\\(W\\|E\\)\\] "
                     1 2))
      (add-to-list 'compilation-error-regexp-alist 'haml-lint))))

;;; packages.el ends here
