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
  '(company
    (company-flow :toggle (configuration-layer/package-usedp 'company))
    (flycheck-flow :toggle (configuration-layer/package-usedp 'flycheck))
    js2-mode
    web-mode))

(defun flow-type/post-init-js2-mode()
  (push 'flow-type/init-mode js2-mode-hook))

(defun flow-type/post-init-web-mode()
  (push 'flow-type/init-mode react-mode-hook))

(defun flow-type/post-init-company()
  (spacemacs|add-company-hook js2-mode)
  (when (configuration-layer/layer-usedp 'react)
    (spacemacs|add-company-hook react-mode)))

(defun flow-type/init-company-flow ()
  (use-package company-flow
    :defer t
    :init
    (progn
       (push 'company-flow company-backends-js2-mode)
       (when (configuration-layer/package-usedp 'web-mode)
         (push 'company-flow company-backends-react-mode))
    )
    :config
    (when (configuration-layer/package-usedp 'web-mode)
      (push 'react-mode company-flow-modes)))
  )

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
        (flycheck-add-next-checker 'javascript-eslint 'javascript-flow))
      )))
