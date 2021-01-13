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
;; line-number-mode is really slow, especially in the GUI 😢
(setq display-line-numbers-type nil)

;; Make window dividers more obvious
(setq window-divider-default-bottom-width 3)

;; use sh for spawning random subprocesses, but fish if you want a proper shell
(setq shell-file-name "/bin/sh")
(let ((fishpath (executable-find "fish")))
  (if fishpath
    (progn
      (setq explicit-shell-file-name fishpath)
      (setq vterm-shell fishpath)
  )))
;; set EDITOR to use the current emacs instance in a shell
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)


;; Disable the weird GUI toolbar I never use
(tool-bar-mode -1)


;; Default window size on startup
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))

;; Hit '-' to jump to the containing directory
(define-key evil-normal-state-map (kbd "-") 'dired-jump)

;; Company completion popups are slow.  Hit C-SPC if you want one
(setq company-idle-delay nil)

(after! magit
  ;; Stop magit complaining about too-long summary lines
  (setq git-commit-style-convention-checks
        (remove 'overlong-summary-line git-commit-style-convention-checks))
  ;; Show timestamps rather than relative dates
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  ;; Try to speed up status buffer refreshes
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  ;; Allegedly helps to hard-code the path rather than force magit to look it up on each execution
  (setq magit-git-executable (executable-find "git"))
  (setq magit-git-executable "git")
  )

(setq projectile-project-search-path '("~/Developer/" "~/Developer/vendor/"))

;; Enter multiedit, then in visual mode hit return to remove all other matches.
;; This is the recommended multiedit keybinding, but doom-emacs doesn't bind it by default.
(after! evil-multiedit
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region))

;; swiper sets a very low max-columns, resulting in "Omitted long line" when searching.
;; https://github.com/abo-abo/swiper/issues/2482
;; Also, rg excludes hidden files by default, which means that searching for stuff in webpack/.storybook/* fails.
;; https://github.com/BurntSushi/ripgrep/issues/623#issuecomment-659909044
(setq counsel-rg-base-command
  "rg --max-columns 500 --with-filename --no-heading --line-number --color never --hidden --glob !.git %s")

;; don't enable smartparens by default - when it doesn't work, it's really frustrating
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

;; Avoid an error in Emacs 27.  Hopefully fixed in more recent builds?
;; https://github.com/seagle0128/doom-modeline/issues/232#issuecomment-544144235
(setq internal-lisp-face-attributes
  [nil
   :family :foundry :swidth :height :weight :slant :underline :inverse
   :foreground :background :stipple :overline :strike :box
   :font :inherit :fontset :vector :extend])

;; Not keen on ruby's auto-formatter, lets just rely on prettier.js for now
(setq +format-on-save-enabled-modes '(js2-mode rjsx-mode typescript-mode typescript-tsx-mode)) ;; css-mode scss-mode))

;; We use prettier to format typescript, so don't want the typescript LSP interfering
(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
(setq-hook! 'js2-mode-hook +format-with-lsp nil)

(after! lsp-mode
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
  (setq lsp-auto-guess-root nil)

  )

;; auto-activate sh-mode for .fish files
(add-to-list 'auto-mode-alist '("\\.fish" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.nix" . nix-mode))

;; don't steal focus when running rspec-compile
(after! enh-ruby-mode
  (set-popup-rule! "\*rspec-compilation\*" :select #'ignore))

(after! haml-mode
  (after! flycheck
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

      (add-to-list 'compilation-error-regexp-alist-alist
                   '(haml-lint
                     "^\\([^:]+\\):\\([0-9]+\\) \\[\\(W\\|E\\)\\] "
                     1 2))
      (add-to-list 'compilation-error-regexp-alist 'haml-lint)))

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
;; I keep getting a flycheck error on trying to create pull requests
(setq flycheck-global-modes '(not forge-post-mode))

;; Don't try to execute 'cvs' when visiting a directory that contains a csv directory
(setq vc-handled-backends '(Git))

(load-file (expand-file-name "mine/cfn-mode.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "mine/hlds-mode.el" (file-name-directory load-file-name)))
