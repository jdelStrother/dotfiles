;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

(package! feature-mode) ;; for cucumber
(package! groovy-mode)
(package! nginx-mode)
(package! foreman-mode)
(package! fish-mode)
(package! just-mode)
(package! yaml-mode)
(package! buffer-terminator)

(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))

(package! string-inflection)

(package! flymake-codespell)

;; (package! jujutsushi
;;   :recipe (:host sourcehut :repo "puercopop/jujutsushi"))
(package! vc-jj
  :recipe (:host codeberg :repo "emacs-jj-vc/vc-jj.el"))
(package! jj-mode :recipe (:host github :repo "bolivier/jj-mode.el"))


;; (package! jj-describe-mode
;;   :recipe (:host github :repo "hrehfeld/emacs-jj-describe-mode"))

;; (package! magit
;;   :recipe
;;   (:host github
;;    :branch "reset-keep"
;;    :repo "jdelStrother/magit"
;;    :files ("*.el" "lisp/*.el")))

;; (package! magit :recipe (:local-repo "path/to/local/magit" :no-byte-compile t))

;; (package! eglot
;;   :recipe
;;   (:host github
;;    :branch "patch-1"
;;    :repo "jdelStrother/eglot"
;;    :files ("*.el")))

(package! benchmark-init)

(package! claude-code :recipe (:host github :repo "stevemolitor/claude-code.el"))
(package! eat)

(package! yasnippet)
