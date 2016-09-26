;;; packages.el --- flow-type layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jonathan del Strother <jon@jons-mbp-4.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst flow-type-packages
  '(flycheck-flow))

(defun flow-type/init-flycheck-flow()
  (use-package flycheck-flow
    :defer t
    :config
    (progn
      ;; Don't run flow if there's no @flow pragma
      (custom-set-variables '(flycheck-javascript-flow-args (quote ("--respect-pragma"))))
      ;; Run flow in react-mode files
      (flycheck-add-mode 'javascript-flow 'react-mode)
      ;; Run flow after eslint
      (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))))
