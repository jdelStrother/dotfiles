;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(php
     systemd
     ansible
     python
     csv
     nginx
     markdown
     sql
     treemacs
     yaml
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     helm
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t)
     ;; better-defaults
     ;; dap
     (lsp :variables lsp-ui-doc-enable nil)
     docker
     emacs-lisp
     ;; heotree
     (git :variables
          git-use-magit-next t
          )
     (github :variables magithub-cache t)
     (go :variables go-format-before-save t)
     gtags
     haml
     helm
     html
     (javascript :variables
                 js2-mode-show-parse-errors nil
                 js2-mode-show-strict-warnings nil
                 node-add-modules-path t
                 javascript-backend 'lsp
                 javascript-fmt-tool 'prettier
                 )
     ;; aj-javascript
     ;; markdown
     osx
     (org :variables
          org-projectile-file (expand-file-name "~/notes.org")
          org-enable-org-journal-support t)
     prettier
     react
     (ruby :variables ruby-test-runner 'rspec ruby-version-manager 'chruby ruby-align-to-stmt-keywords '(def if) ruby-backend 'lsp)
     shell
     shell-scripts
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     templates
     terraform
     typescript
     vinegar
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(scss-mode rvm groovy-mode base16-theme gif-screencast gcmh)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   ;; Remove jinja2 because its syntax highlighting is very geared towards html, not unix config files
   dotspacemacs-excluded-packages '(smartparens evil-unimpaired company-tern tern rainbow-delimiters jinja2 magit-gitflow)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper t

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "/Applications/Emacs.app/Contents/MacOS/Emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai base16-default-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 11
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; https://github.com/syl20bnr/spacemacs/issues/3920
  (setq exec-path-from-shell-arguments '("-l"))

  (add-to-list 'default-frame-alist '(width . 160))
  (add-to-list 'default-frame-alist '(height . 50))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

  (setq node-add-modules-path t)
  (setq-default git-magit-status-fullscreen t)
  ;; We don't need VC if we have magit
  (setq vc-handled-backends nil)

  (setq lsp-eslint-server-command
        '("node"
          "/Users/jon/.vscode/extensions/dbaeumer.vscode-eslint-2.0.11/server/out/eslintServer.js"
          "--stdio"))

  (require 'gnutls)
  (add-to-list 'gnutls-trustfiles "/usr/local/etc/openssl/cert.pem")

  (setq custom-file (concat user-emacs-directory "custom.el"))
  (load custom-file)
  )
(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  (with-temp-buffer
    (helm-mode))
  (with-temp-buffer
    (require 'magit)
    (magit-mode))
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; Try the garbage collection hack http://akrl.sdf.org/#org2a987f7
  (require 'gcmh)
  (gcmh-mode 1)

  ; Treat _ as part of a word
  (modify-syntax-entry ?_ "w")
  ; Make :s/foo/bar global by default
  (setq evil-ex-substitute-global t)

  ; Save all on focus-loss
  (add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

  ;; sane indentation
  (setq-default standard-indent 2)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default evil-shift-width 2)
  (setq js-indent-level 2)
  (setq css-indent-offset 2)
  (setq haml-indent-offset 2)
  (setq js2-basic-offset 2)
  (setq js2-indent-switch-body 1)

  ;; don't use GUI popup alerts
  (setq use-dialog-box nil)

  ;; stop asking about following symlinks when editing home-dir-dotfiles
  (setq vc-follow-symlinks nil)

  ;;(load-file ".emacs.d/private/sgml-mode-patch.el")
  ;;(require 'sgml-mode)

  ;; fix copy-paste? https://github.com/syl20bnr/spacemacs/issues/2032
  ;; (fset 'evil-visual-update-x-selection 'ignore)

  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-auto-quoting nil)
  )
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  (add-to-list 'auto-mode-alist '("\\.es6" . js2-mode))

  ;; (setq dap-chrome-debug-program `("node" ,(expand-file-name "~/.extensions/chrome/out/src/chromeDebug.js")))

  (with-eval-after-load 'flycheck
    ;; disable jshint since we prefer eslint checking
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint
                            scss
                            scss-lint
                            ))
                  )
    ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-flow-coverage)

    ;; flow-coverage shows up as warnings, which is very noisy.
    ;; Make the warning markers more subtle, and hide the gutter dot.
    (set-face-attribute 'flycheck-fringe-warning nil :foreground (face-attribute 'fringe :background ))
    (set-face-attribute 'flycheck-warning nil :underline (color-lighten-name (face-background 'default) 20))
  )

  (add-hook 'js2-mode-hook 'prettier-js-mode)
  ;; (add-hook 'web-mode-hook 'prettier-js-mode)

  ;; (setq-default flycheck-idle-change-delay 2.5)
  (setq-default flycheck-check-syntax-automatically '(mode-enabled save))
  (setq-default flycheck-javascript-flow-args (quote ("--respect-pragma")))
  ;; `yarn global add eslint_d`
  (setq flycheck-javascript-eslint-executable "eslint_d")

  (defun bundle-exec-flycheck ()
    (make-local-variable 'flycheck-command-wrapper-function)
    (setq flycheck-command-wrapper-function (lambda (command) (append '("bundle" "exec") command))))
  (add-hook 'haml-mode-hook 'bundle-exec-flycheck)

  (setq flycheck-ruby-rubocop-executable "rubocop-daemon-wrapper")


  ;; Don't ask whether to execute code in org files
  (setq org-confirm-babel-evaluate nil)

  (add-hook 'haml-mode-hook 'rvm-activate-corresponding-ruby)

  (defun github-browse-file--relative-url--with-hacks (orig-fun &rest args)
    ;; call the original function (getting the origin-remote's url),
    ;; and if that's not present, look for a github-remote.
    (or
     (apply orig-fun args)
     (let ((url (vc-git--run-command-string nil "config" "remote.github.url")))
       (when (and url (string-match "github.com:?/?\\(.*\\)" url))
         (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))
    )

  (advice-add 'github-browse-file--relative-url :around #'github-browse-file--relative-url--with-hacks)

  ;; C-x C-l to expand a matching line
  (defun my-expand-lines ()
    (interactive)
    (let ((hippie-expand-try-functions-list
           '(try-expand-line)))
      (call-interactively 'hippie-expand)))

  (define-key evil-insert-state-map (kbd "C-x C-l") 'my-expand-lines)
  (global-set-key (kbd "C-x C-p") 'profiler-report)

  ;; cmd-c / cmd-v
  ;; (define-key global-map "\M-c" 'ns-copy-including-secondary)
  ;; (define-key global-map "\M-v" 'yank)

  (defadvice rspec-compile (around rspec-compile-around)
    ;; DISABLE_SPRING=1 rake 'parallel:prepare[5]'
    ;; Use BASH shell for running the specs because of ZSH issues.
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile)

  (defun compilation-exit-autoclose (STATUS code msg)
    "Close the compilation window if there was no error at all."
    ;; If M-x compile exists with a 0
    (when (and (eq STATUS 'exit) (zerop code))
      ;; then bury the *compilation* buffer, so that C-x b doesn't go there
      (bury-buffer)
      ;; and delete the *compilation* window
      (delete-window (get-buffer-window (get-buffer "*compilation*"))))
    ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code))
  (setq compilation-exit-message-function 'compilation-exit-autoclose)


  ;; (eval-after-load 'rspec-mode
  ;;   ;; I got frustrated trying to find a combination that uses spring in docker,
  ;;   ;; while also avoiding 'risky' emacs variables prompt every time.
  ;;   '(defun rspec-compile-command (target &optional opts)
  ;;      "Composes RSpec command line for the compile function"
  ;;      (format "docker exec web_web_1 bin/rspec %s"
  ;;              (mapconcat 'identity `(,(rspec-runner-options opts)
  ;;                                     ,(rspec-runner-target target)) " "))))

  ;; (with-eval-after-load 'rspec-mode
  ;;   ;; (require 'dap-ruby)
  ;;   (defun jds-rspec--populate-start-file-args (conf)
  ;;     "Populate CONF with the required arguments."
  ;;     (progn
  ;;       (-> conf
  ;;           (dap--put-if-absent :dap-server-path dap-ruby-debug-program)
  ;;           (dap--put-if-absent :cwd (rspec-project-root))
  ;;           (dap--put-if-absent :args (list (buffer-file-name)))
  ;;           (dap--put-if-absent :type "Rspec")
  ;;           (dap--put-if-absent :name "Rspec Debug"))
  ;;       (message "%s" conf)
  ;;       conf))
  ;;   (message "Registering templates!")
  ;;   (dap-register-debug-provider "Ruby" 'jds-rspec--populate-start-file-args)
  ;;   (dap-register-debug-template "Rspec Run Configuration!"
  ;;                               (list :type "Ruby"
  ;;                                     :cwd "/Users/jon/Developer/web"
  ;;                                     :request "launch"
  ;;                                     :program "bin/rspec"
  ;;                                     :environment-variables '(("DISABLE_SPRING" . "1"))
  ;;                                     :name "Ruby::Rspec"))
  ;;   )
  ;; (add-hook 'ruby-mode-hook (lambda () (spacemacs/dap-bind-keys-for-mode 'ruby-mode)))

  (evil-set-register ?i
        (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([99 119 105 109 112 111 114 116 escape 47 61 return 99 102 40 105 109 backspace backspace 102 114 111 109 32 escape 36 120 106 94] 0 "%d")) arg)))

  ;; Fix "ls does not support --dired" warning
  (let ((gls (executable-find "gls")))
    (when gls
      (setq insert-directory-program gls
            dired-listing-switches "-aBhl --group-directories-first")))

  ;; (with-eval-after-load 'org-agenda
  ;;   (require 'org-projectile)
  ;;   (push (org-projectile:todo-files) org-agenda-files))


  ;; Magical find-all-stuff helm
  (setq helm-for-files-preferred-list '(helm-source-buffers-list
                                        ;; helm-source-buffer-not-found
                                        helm-source-projectile-projects
                                        helm-source-projectile-files-list
                                        helm-source-recentf
                                        helm-source-bookmarks
                                        helm-source-file-cache
                                        ;; helm-source-files-in-current-dir
                                        ))
  (define-key evil-normal-state-map (kbd "C-p") 'helm-multi-files)
  ;; don't open helm in a new frame
  (setq helm-use-frame-when-more-than-two-windows nil)
  (setq projectile-enable-caching t)
  (setq helm-recentf-fuzzy-match t)

  ;; Include underscore as part of word
  (add-hook 'ruby-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'js2-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  (add-hook 'haml-mode-hook (lambda () (modify-syntax-entry ?_ "w")))

  (defun ruby-simple-function-name ()
    (car (last (split-string (magit-which-function) "#"))))
  (add-hook 'ruby-mode-hook (lambda () (setq magit-log-trace-definition-function 'ruby-simple-function-name)))

  (setq org-agenda-files (list (expand-file-name "~/notes.org") (expand-file-name "~/Dropbox/workflow-notes")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

  ;; Just autoload TAGS, don't ask
  ;; (setq tags-revert-without-query 1)

  ;; (tramp-set-completion-function "ssh"
  ;;                                '((tramp-parse-sconfig "/etc/ssh_config")
  ;;                                  (tramp-parse-sconfig "~/.ssh/config")))


  (with-eval-after-load 'gif-screencast
    ;; I can't persuade cropping to work in GUI emacs - just use in iTerm2
    (let* ((app-name (if (eq window-system 'ns) "Emacs" "iTerm2"))
           (id (shell-command-to-string (format "osascript -e 'tell app \"%s\" to id of window 1'" app-name))))
      (setq gif-screencast-args (list "-x" "-o" "-w" (format "-l%s" (substring id 0 -1))))
    )
    (setq gif-screencast-optimize-args '("--batch" "--optimize=3" "--colors=256"))
    (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))


  ;; Allegedly you can just set magit-wip-mode in Custom, but I can't get that working.  Manually require it.
  ;; Update: I don't like it, it makes saves too slow
  ;; (require 'magit-wip)
  ;; (magit-wip-mode t)

  ;; I'm not keen on the LSP sideline flashing up constantly while typing.  Disable while in insert mode.
  (add-hook 'lsp-mode-hook (lambda()
    (let ((original-lsp-sideline-value nil))
      (make-local-variable 'original-lsp-sideline-value)
      (add-hook 'evil-insert-state-entry-hook (lambda () (progn
        (setq original-lsp-sideline-value lsp-ui-sideline-mode)
        (lsp-ui-sideline-enable nil))))
      (add-hook 'evil-insert-state-exit-hook (lambda ()
        (lsp-ui-sideline-enable original-lsp-sideline-value))))))
)
