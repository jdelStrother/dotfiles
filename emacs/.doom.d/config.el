;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Jonathan del Strother"
      user-mail-address "jdelStrother@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Disable the weird GUI toolbar I never use
(tool-bar-mode -1)

;; Default window size on startup
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))

;; Hit '-' to jump to the containing directory
(define-key evil-normal-state-map (kbd "-") 'dired-jump)

;; Avoid an error in Emacs 27.  Hopefully fixed in more recent builds?
;; https://github.com/seagle0128/doom-modeline/issues/232#issuecomment-544144235
(setq internal-lisp-face-attributes
  [nil
   :family :foundry :swidth :height :weight :slant :underline :inverse
   :foreground :background :stipple :overline :strike :box
   :font :inherit :fontset :vector :extend])

;; Not keen on ruby's auto-formatter, lets just rely on prettier.js for now
(setq +format-on-save-enabled-modes '(js2-mode))

(after! lsp
  ;; I'm not keen on the LSP sideline flashing up constantly while typing.  Disable while in insert mode.
  (add-hook 'lsp-mode-hook (lambda()
    (let ((original-lsp-sideline-value nil))
      (make-local-variable 'original-lsp-sideline-value)
      (add-hook 'evil-insert-state-entry-hook (lambda () (progn
        (setq original-lsp-sideline-value lsp-ui-sideline-mode)
        (lsp-ui-sideline-enable nil))))
      (add-hook 'evil-insert-state-exit-hook (lambda ()
        (lsp-ui-sideline-enable original-lsp-sideline-value))))))
  ;; I can't get lsp to correctly use our webpack subdirectory as a project if auto-guess-root is enabled
  (setq lsp-auto-guess-root nil))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
