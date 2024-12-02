;;; ansible-lint.el ---                              -*- lexical-binding: t; -*-

;; Someday maybe we'll get some activity on https://github.com/flycheck/flycheck/pull/1858 and this will become redundant
;; https://github.com/jdelStrother/flycheck/blob/168a487bd5d3c9a8b980892cd7be4145861904a3/flycheck.el#L7468

(after! flycheck
  (defun flycheck-ansible--ansible-lint-find-default-directory (_checker)
    " Find a parent directory containing a .ansible-lint file"
    (when (buffer-file-name)
      (locate-dominating-file (file-name-directory (buffer-file-name)) ".ansible-lint")))

  (flycheck-define-checker ansible-ansiblelint
    "An Ansible linter using the ansible-lint tool.
        See URL `https://ansible-lint.readthedocs.io/en/latest/'."
    :enabled (lambda () (bound-and-true-p ansible-mode))
    :command ("ansible-lint" "--offline" "-v" "--nocolor" "-p" source-original)
    :working-directory flycheck-ansible--ansible-lint-find-default-directory
    :predicate flycheck-buffer-saved-p
    :error-patterns
    (
     ;; ansible-lint v4 output
     ;; (error "CRITICAL Couldn't parse task at " (file-name) ":" line " " (message))
     ;; (warning line-start (file-name) ":" line ": [E" (id (+ digit)) "] " (message)
     ;;          line-end)
     ;; ansible-lint v5 output
     (error line-start (file-name) ":" line (optional ":" column) ": "
            (id (or "syntax-check" "internal-error" "parser-error" "load-failure"))
            ":" (message) line-end)
     (warning line-start (file-name) ":" line (optional ":" column) ": "
              (id (+ (any "a-z-[]"))) ":" (message) line-end)
     )
    :error-explainer
    (lambda (err)
      (let* ((id (flycheck-error-id err))
             (lines (process-lines "ansible-lint" "-L" "-f" "plain"))
             ;; process-lines may flush output before the entire line of the rule
             ;; is ready; reconstruct the desired error to a single line
             (start (+ 1 (seq-position
                          lines t
                          (lambda (elt _)
                            (string-prefix-p id elt)))))
             (next-id (car (seq-filter
                            (lambda (elt)
                              (string-match-p "^[0-9a-z-]+: " elt))
                            (nthcdr start lines))))
             (end (if (not next-id)
                      (length lines)
                    (seq-position
                     lines t
                     (lambda (elt _)
                       (string-prefix-p next-id elt))))))
        (substring (mapconcat 'identity (seq-subseq lines start end) " ") 2)))
    :modes yaml-mode
    :next-checkers ((t . yaml-jsyaml)
                    (t . yaml-ruby)
                    (t . yaml-yamllint)))

  (add-to-list 'flycheck-checkers 'ansible-ansiblelint))
