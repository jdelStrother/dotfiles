(defun flow-type/call-process-on-buffer-to-string (command)
  (with-output-to-string
    (call-process-region (point-min) (point-max) shell-file-name nil standard-output nil shell-command-switch command)))

(defun flow-type/type-description (info)
  (let ((type (alist-get 'type info)))
    (if (string-equal type "(unknown)")
        (let ((reasons (alist-get 'reasons info)))
          (if (> (length reasons) 0) (alist-get 'desc (aref reasons 0))))
      type)))

(defun flow-type/type-at-cursor ()
  (let* ((info (json-read-from-string
                (flow-type/call-process-on-buffer-to-string
                (format "flow type-at-pos --json  %d %d" (line-number-at-pos) (+ (current-column) 1)))))
        (type (flow-type/type-description info)))
    type))

(defun flow-type/init-mode ()
  (set (make-local-variable 'eldoc-documentation-function) 'flow-type/type-at-cursor))
