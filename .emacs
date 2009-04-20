;; Add ~/.elisp & all subdirs to load path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
       (let* ((my-lisp-dir "~/.elisp/")
             (default-directory my-lisp-dir))
          (setq load-path (cons my-lisp-dir load-path))
          (normal-top-level-add-subdirs-to-load-path)))

(require 'git)
(require 'gitsum)
(autoload 'git-blame-mode "git-blame"
          "Minor mode for incremental blame for Git." t)
