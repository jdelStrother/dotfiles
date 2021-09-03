;;; frame-recenter.el ---                            -*- lexical-binding: t; -*-

;; (defun my/frame-recenter (&optional frame)
;;   "Center FRAME on the screen.
;; FRAME can be a frame name, a terminal name, or a frame.
;; If FRAME is omitted or nil, use currently selected frame."
;;   (interactive)
;;   (message "recentering")
;;   (unless (eq 'maximised (frame-parameter nil 'fullscreen))
;;     (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
;;             (frame-w (frame-pixel-width frame))
;;             (frame-h (frame-pixel-height frame))
;;             ;; frame-monitor-workarea returns (x y width height) for the monitor
;;             (monitor-w (nth 2 (frame-monitor-workarea frame)))
;;             (monitor-h (nth 3 (frame-monitor-workarea frame)))
;;             (center (list (/ (- monitor-w frame-w) 2)
;;                           (/ (- monitor-h frame-h) 2))))
;;       (message "set frame %s %s" frame center)
;;       (apply 'set-frame-position (flatten-list (list frame center))))))

;; (add-hook 'after-init-hook #'my/frame-recenter)
;; (add-hook 'after-make-frame-functions #'my/frame-recenter)


(let ((width 1000)
       (height 700)
       (display-height (display-pixel-height))
       (display-width  (display-pixel-width)))
   (add-to-list 'default-frame-alist `(left . ,(- (/ display-width 2) (/ width 2))))
   (add-to-list 'default-frame-alist `(top . ,(- (/ display-height 2) (/ height 2))))
   (add-to-list 'default-frame-alist `(width . (text-pixels . ,width)))
   (add-to-list 'default-frame-alist `(height . (text-pixels . ,height))))
