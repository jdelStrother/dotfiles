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
  '(flycheck
    flycheck-flow
    js2-mode
    react-mode))

(defun flow-type/post-init-js2-mode()
  (flow-type/setup-timer))

(defun flow-type/post-init-react-mode()
  (flow-type/setup-timer))

(defun flow-type/init-flycheck-flow()
  (with-eval-after-load 'flycheck
    (use-package flycheck-flow
      :config
      (progn
        ;; Don't run flow if there's no @flow pragma
        (custom-set-variables '(flycheck-javascript-flow-args (quote ("--respect-pragma"))))
        ;; Run flow in react-mode files
        (flycheck-add-mode 'javascript-flow 'react-mode)
        ;; Run flow after eslint
        (flycheck-add-next-checker 'javascript-eslint 'javascript-flow)))))
