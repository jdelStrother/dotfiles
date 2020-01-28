(defun flow-type/known-type-at-pos ()
  ;; You'll get '(unknown)' while cursoring over comments, whitespace, keywords, etc
  ;; Don't bother reporting type information for those instances:

  (let ((type (flow-minor-type-at-pos)))
    (if (not (string-match "^\\(flow is still initializing\\|(unknown)\\)" type))
        type)))


(defun flow-type/known-type-at-pos-async ()
  (let
    ((libpath (cdr (find-function-library 'flow-minor-type-at-pos)))
    (file (buffer-file-name))
    (line (number-to-string (line-number-at-pos)))
    (col (number-to-string (1+ (current-column)))))
  (async-start
   `(lambda ()
      (set 'libpath ,libpath)
      (set 'file ,file)
      (set 'line ,line)
      (set 'col ,col)
     (load-file libpath)
     (flow-minor-with-flow
      (let ((type (flow-minor-cmd-to-string "type-at-pos" "--quiet" file line col)))
      (flow-minor-colorize-type (car (split-string type "\n"))))))
   (lambda(result) (eldoc-message result))
  )))


(defun flow-type/enable-eldoc ()
  (if (and flow-type-enable-eldoc-type-info (flow-minor-configured-p))
      (
       ;; set (make-local-variable 'eldoc-documentation-function) 'flow-type/known-type-at-pos-async
      )
     ))
