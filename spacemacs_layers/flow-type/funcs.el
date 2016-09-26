(defun call-process-on-buffer-to-string (command)
  (with-output-to-string
    (call-process-region (point-min) (point-max) shell-file-name nil standard-output nil shell-command-switch command)))


(defun flow-type-description (info)
  (let ((type (alist-get 'type info)))
    (if (string-equal type "(unknown)")
        (let ((reasons (alist-get 'reasons info)))
          (if (> (length reasons) 0) (alist-get 'desc (aref reasons 0))))
      type)))


(defun flow-type ()
  (interactive)
  (let* ((info (json-read-from-string
                (call-process-on-buffer-to-string
                 (format "flow type-at-pos --json  %d %d" (line-number-at-pos) (+ (current-column) 1)))))
         (type (flow-type-description info)))
    (if type (spacemacs/echo "%s" type))))


(defun flow-pragma-exists ()
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (null (search-forward "@flow" 200 t))))))

(defvar flow-type-global-timer nil
  "Timer to trigger flow-typing.")

(defun flow-type-at-point ()
  (when (flow-pragma-exists)
    (flow-type)
    ))

(defun flow-type-setup-timer ()
  (unless flow-type-global-timer
    (setq flow-type-global-timer
          (run-with-idle-timer 0.5 :repeat 'flow-type-at-point))))
