;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Jonathan del Strother"
      user-mail-address "jdelStrother@gmail.com")

;; automatically revert buffers if they change on disk
(global-auto-revert-mode t)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Source Code Pro" :size 13))


;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; I needed this for an MS Sculpt keyboard's M-x to work
(setq ns-right-option-modifier 'left)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; Hit `SPC t l` to toggle relative line numbers on
(setq display-line-numbers-type nil)

(setq confirm-kill-emacs nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")
;; (setq org-roam-directory "~/Documents/org/roam")
(setq org-agenda-files '("~/Documents/org/"
                         ;; "~/Documents/org/roam" "~/Documents/org/roam/daily"
                         "~/Library/Mobile Documents/iCloud~com~agiletortoise~Drafts5/Documents/org"
                         "~/Library/Mobile Documents/iCloud~is~workflow~my~workflows/Documents/inbox.org"))
;; hide todos that are deferred to future dates
(setq org-agenda-todo-ignore-scheduled 'future)
(setq org-log-done 'time)
(after! org
  ;; auto-save after toggling todo state
  (add-hook 'org-trigger-hook 'save-buffer)
  (org-link-set-parameters "message" :follow
                           (lambda (id)
                             (shell-command
                              (concat "open message:" id))))

  ;; Doom's default capture templates, but excluding the project-specific ones I never use,
  ;; and with a timestamp on todos
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a\n%u\n\n" :prepend t)
          ("n" "Notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t))))

;; I never touch Emac's gross menubar mess
(menu-bar-mode -1)

;; Better insert behaviour with evil
;; https://github.com/syl20bnr/spacemacs/issues/14137
(defadvice org-roam-insert (around append-if-in-evil-normal-mode activate compile)
  "If in evil normal mode and cursor is on a whitespace character, then go into
append mode first before inserting the link. This is to put the link after the
space rather than before."
  (let ((is-in-evil-normal-mode (and (bound-and-true-p evil-mode)
                                     (not (bound-and-true-p evil-insert-state-minor-mode))
                                     (looking-at "[[:blank:]]"))))
    (if (not is-in-evil-normal-mode)
        ad-do-it
      (evil-append 0)
      ad-do-it
      (evil-normal-state))))

(map! :leader :desc "capture today" "n r C" #'org-roam-dailies-capture-today)

;; Make window dividers more obvious
(setq window-divider-default-bottom-width 3)

;; use bash for spawning random subprocesses, but fish if you want a proper shell
(setq shell-file-name (executable-find "bash"))
(let ((fishpath (executable-find "fish")))
  (if fishpath
      (progn
        (setq explicit-shell-file-name fishpath)
        (setq vterm-shell fishpath)
        )))
;; set EDITOR to use the current emacs instance in a shell
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)


;; Default window size on startup
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Hit '-' to jump to the containing directory
(define-key evil-normal-state-map (kbd "-") 'dired-jump)

(setq-default tab-width 2)
(setq-default scroll-margin 5)
(pixel-scroll-precision-mode)

;; Company completion popups are slow.  Hit C-SPC if you want one
;;
(when (modulep! :completion company)
  (setq company-idle-delay nil))

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
  )

(setq projectile-project-search-path '("~/Developer/" "~/Developer/vendor/"))
(setq projectile-rails-expand-snippet-with-magic-comment t)
;; Enter multiedit, then in visual mode hit return to remove all other matches.
;; This is the recommended multiedit keybinding, but doom-emacs doesn't bind it by default.
(after! evil-multiedit
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region))

;; don't enable smartparens by default - when it doesn't work, it's really frustrating
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)


;; If we're using rubocop in apheleia, use bundle-exec and replace the deprecated --auto-correct flag
;; (Normally this won't get used - LSP will format it via solargraph/ruby-lsp instead)
(after! apheleia
  (add-to-list 'apheleia-formatters
               '(rubocop . ("bundle" "exec" "rubocop" "--stdin" filepath "--autocorrect"
                            "--stderr" "--format" "quiet" "--fail-level" "fatal"))))

;; disabling apheleia in web-mode, because it then tries to format all .html.erb files with npx-prettier.
;; I'd tried to define a new erb derived mode below, but no luck with it yet
(setq-hook! 'web-mode-hook
  apheleia-inhibit t
  +format-with nil)

(after! web-mode
  ;; add a derived erb-mode so that flycheck/apheleia can distinguish it from all the other web-mode files
  (define-derived-mode erb-mode web-mode "Web[erb]")
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . erb-mode)))

;; (after! apheleia
;;   (push '(erblint . ("bundle" "exec" "erblint" "--autocorrect" inplace ))
;;         apheleia-formatters)
;;   (setf (alist-get 'erb-mode apheleia-mode-alist)
;;         '(erblint))
;;   )


(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(after! lsp-mode
  (setq lsp-restart 'ignore) ;; don't prompt to restart a bunch of lsp servers every time I kill a project
  ;; I can't get lsp to correctly use our webpack subdirectory as a project if auto-guess-root is enabled.
  ;; Use lsp-workspace-folders-add instead.
  (setq lsp-auto-guess-root nil)
  (add-function :around (symbol-function 'lsp-file-watch-ignored-directories)
                (lambda (orig)
                  (let ((root (lsp--workspace-root (cl-first (lsp-workspaces)))))
                    (cond
                     ((string-prefix-p "/Users/jon/Developer/web" root)
                      (append '("/tmp$" "/vendor$" "/webpack$" "/files$" "app/assets/javascripts/packages" "") (funcall orig)))
                     (t
                      (funcall orig))))))

  ;; steep tries to activate if 'bundle' is present in the path
  ;; disable ruby-ls (solargraph gem) in favour of ruby-lsp-ls (ruby-lsp gem)
  (setq lsp-disabled-clients '(steep-ls typeprof-ls ruby-ls))

  (setq lsp-warn-no-matched-clients nil)

  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '(
                  "[/\\\\]\\.direnv\\'"
                  ;; Bunch of audioboom-specific things because lsp doesn't work great with dir-locals (https://www.reddit.com/r/emacs/comments/jeenx4/til_how_to_load_file_or_dirlocals_before_a_minor/)
                  "[/\\\\]\\.sass-cache\\'"
                  "[/\\\\]\\.services\\'"
                  "[/\\\\]app/assets/javascripts/packages\\'"
                  "[/\\\\]coverage\\'"
                  "[/\\\\]db/sphinx\\'"
                  "[/\\\\]files\\'"
                  "[/\\\\]tmp\\'"
                  "[/\\\\]vendor\\'"
                  "[/\\\\]webpack\\'"
                  )))

  (lsp-defun my/filter-typescript ((params &as &PublishDiagnosticsParams :diagnostics)
                                   _workspace)
    (lsp:set-publish-diagnostics-params-diagnostics
     params
     (or (seq-filter (-lambda ((&Diagnostic :source? :code?))
                       ;; Silence "This may be converted to an async function" from typescript
                       (not (and (eq 80006 code?) (string= "typescript" source?))))
                     diagnostics)
         []))
    params)

  (setq lsp-diagnostic-filter 'my/filter-typescript )

  ;; I'm not keen on the LSP sideline flashing up constantly while typing. Disable while in insert mode.
  (add-hook 'lsp-mode-hook
            (lambda()
              (setq-local original-lsp-sideline-value nil)
              (add-hook 'evil-insert-state-entry-hook
                        (lambda ()
                          "Save the original sideline value and then disable it"
                          (setq-local original-lsp-sideline-value lsp-ui-sideline-mode)
                          (lsp-ui-sideline-enable nil))
                        nil t)
              (add-hook 'evil-insert-state-exit-hook
                        (lambda ()
                          "Restore the original sideline value"
                          (lsp-ui-sideline-enable original-lsp-sideline-value))
                        nil t)))
  )


(add-to-list 'auto-mode-alist '("\\.nix" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.mdx" . markdown-mode))

;; don't steal focus when running rspec-compile
(after! rspec-mode
  (set-popup-rule! "\*rspec-compilation\*" :select #'ignore)
  (defun rspec-run-all-failed ()
    "Run the `spec' rake task for the project of the current file."
    (interactive)
    (rspec-run (concat (rspec-core-options) " --only-failures ")))
  (map! :localleader
        :prefix "t"
        :map (rspec-verifiable-mode-map rspec-mode-map)
        "F" #'rspec-run-all-failed)

  ;; i usually just run single specs in emacs, skip the formatters in .rspec
  (setq rspec-use-opts-file-when-available nil)
  (add-hook 'rspec-compilation-mode-hook (lambda () (text-scale-decrease 1)))
  )

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

;; load-file-name is used when we're loaded normally, buffer-file-name for if we eval this buffer
(let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
  (load-file (expand-file-name "mine/cfn-mode.el" dir))
  (load-file (expand-file-name "mine/hlds-mode.el" dir))
  (load-file (expand-file-name "mine/editor-frame.el" dir))
  (load-file (expand-file-name "mine/frame-recenter.el" dir))
  (load-file (expand-file-name "mine/ansible-lint.el" dir))
  (load-file (expand-file-name "mine/clipboard.el" dir)))

;; (explain-pause-mode t)

;; (use-package eglot
;;   :config
;;   (setq eglot-connect-timeout 60) ;; solargraph is slow to start
;;   (map-put eglot-server-programs '(js-mode js2-mode rjsx-mode) '("flow" "lsp" "--from" "emacs" "--autostop"))
;;   )

(after! project
  (defvar project-root-markers '("package.json")
    "Files or directories that indicate the root of a project.")
  (defun jds/project-find-root (path)
    "Tail-recursive search in PATH for root markers."
    (let* ((this-dir (file-name-as-directory (file-truename path)))
           (parent-dir (expand-file-name (concat this-dir "../")))
           (system-root-dir (expand-file-name "/")))
      (cond
       ((jds/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (jds/project-find-root parent-dir)))))
  (defun jds/project-root-p (path)
    "Check if current PATH has any of project root markers."
    (let ((results (mapcar (lambda (marker)
                             (file-exists-p (concat path marker)))
                           project-root-markers)))
      (eval `(or ,@ results))))
  (add-to-list 'project-find-functions #'jds/project-find-root))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(flycheck-pos-tip pos-tip)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; (defun doom-thing-at-point-or-region (&optional thing prompt)
;;   "Grab the current selection, THING at point, or xref identifier at point.

;; Returns THING if it is a string. Otherwise, if nothing is found at point and
;; PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
;; the prompting string). Returns nil if all else fails.

;; NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
;; in some cases."
;;   (declare (side-effect-free t))
;;   (cond ((stringp thing)
;;          thing)
;;         ((doom-region-active-p)
;;          (buffer-substring-no-properties
;;           (doom-region-beginning)
;;           (doom-region-end)))
;;         (thing
;;          (thing-at-point thing t))
;;         ((require 'xref nil t)
;;          ;; A little smarter than using `symbol-at-point', though in most cases,
;;          ;; xref ends up using `symbol-at-point' anyway.
;;          ;; "Most cases" doesn't cover 'eglot so we manually exclude it.
;;          ;; See discussion in https://github.com/joaotavora/eglot/issues/503
;;          (xref-backend-identifier-at-point (xref-find-backend)))
;;         (prompt
;;          (read-string (if (stringp prompt) prompt "")))))

;; (defun +jdelStrother/search-project-for-symbol-at-point (&optional symbol arg)
;;   "Search current project for symbol at point.
;; If prefix ARG is set, prompt for a known project to search from."
;;   (interactive
;; ;; Only change
;;    (list (rxt-quote-pcre (or (doom-thing-at-point-or-region 'symbol) ""))
;;          current-prefix-arg))
;;   (let* ((projectile-project-root nil)
;;          (default-directory
;;            (if arg
;;                (if-let (projects (projectile-relevant-known-projects))
;;                    (completing-read "Search project: " projects nil t)
;;                  (user-error "There are no known projects"))
;;              default-directory)))
;;     (cond ((featurep! :completion ivy)
;;            (+ivy/project-search nil symbol))
;;           ((featurep! :completion helm)
;;            (+helm/project-search nil symbol))
;;           ((rgrep (regexp-quote symbol))))))

;; (map! :leader
;; :desc "Search for symbol in project" "*" #'+jdelStrother/search-project-for-symbol-at-point)

;; (use-package! dash)


(defun doom/ediff-init-and-example ()
  "ediff the current `init.el' with the example in doom-emacs-dir"
  (interactive)
  (ediff-files (concat doom-user-dir "init.el")
               (concat doom-emacs-dir "templates/init.example.el")))

(define-key! help-map
  "di"   #'doom/ediff-init-and-example
  )

;; (defun maybe-use-prettier ()
;;   "Enable prettier-js-mode if an rc file is located."
;;   (if (locate-dominating-file default-directory ".prettierrc")
;;       (prettier-mode +1)))

;; (use-package prettier
;;   :hook ((typescript-mode . maybe-use-prettier)
;;          (typescript-ts-mode . maybe-use-prettier)
;;          (tsx-ts-mode . maybe-use-prettier)
;;          (js-mode . maybe-use-prettier)
;;          (css-mode . maybe-use-prettier)
;;          (scss-mode . maybe-use-prettier)
;;          (json-mode . maybe-use-prettier)
;;          ;; (ruby-mode . maybe-use-prettier)
;;          (web-mode . maybe-use-prettier)
;;          (yaml-mode . maybe-use-prettier)))

(after! robe
  ;; Disable robe - i don't like it prompting me to launch a console on every jump-to-definition,
  ;; and if i let it launch a console it makes a mess of autocompletion suggestions
  (defun robe-jump (_)))

;; Avoid leaving a million dired buffers when navigating directories
(setq dired-kill-when-opening-new-dired-buffer t)

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (add-hook! '(typescript-ts-mode-hook tsx-ts-mode-hook) #'lsp!))


;; -- String inflection: underscore -> UPCASE -> CamelCase conversion of names
;; https://github.com/akicho8/string-inflection

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-java-style-cycle
             string-inflection-python-style-cycle
             string-inflection-elixir-style-cycle
             string-inflection-ruby-style-cycle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase
             string-inflection-kebab-case)

  :init
  (map! :prefix ("g SPC" . "Convert case")
        :desc "cycle"              :nv "n"     #'string-inflection-all-cycle
        :desc "toggle"             :nv "t"     #'string-inflection-toggle
        :desc "PascalCase"         :nv "p"     #'string-inflection-camelcase
        :desc "camelCase"          :nv "c"     #'string-inflection-lower-camelcase
        :desc "kebab-case"         :nv "k"     #'string-inflection-kebab-case
        :desc "snake_case"         :nv "s"     #'string-inflection-underscore
        :desc "Capital_Snake_Case" :nv "S"     #'string-inflection-capital-underscore
        :desc "UP_CASE"            :nv "u"     #'string-inflection-upcase))

(use-package flymake-codespell
  :ensure t
  :hook (prog-mode . flymake-codespell-setup-backend))
(add-hook 'prog-mode-hook #'flymake-mode)
(after! lsp-mode
  (setq lsp-diagnostics-provider :flymake))
;; flymake reports a lot of bytecompile errors in my emacs config files which I'm not sure I care abut
(add-hook 'emacs-lisp-mode-hook
          (defun my-elisp-flymake-hook ()
            (when (doom-real-buffer-p (current-buffer))
              (when (seq-find (lambda (dir) (file-in-directory-p (buffer-file-name) dir))
                              '("~/.config" "~/.doom.d" "~/.emacs.d/lisp" "~/.emacs.d/modules"))
                (setq flymake-diagnostic-functions '(flymake-codespell-backend)))
              (flymake-mode))))

