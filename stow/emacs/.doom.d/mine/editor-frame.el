;;; editor-frame.el --- create a new frame, locked to a single throwaway project  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Jonathan del Strother

;; Author: Jonathan del Strother <jdelStrother@gmail.com>
;;
;; Launched via https://github.com/jdelStrother/dotfiles/blob/master/bin/edit

(defun jds-make-editor-frame (dir initialfile &optional linenumber)
  ;; using make-frame rather than 'emacsclient --create-frame' because the latter is a bit weird on macOS,
  ;; creating a second dock icon with the generic file icon rather than an emacs icon, and doesn't allow cmd-~ to switch windows
  (let ((frame (make-frame '((display . ":0"))))
        (frame-count (length (frame-list))))
    ;; center it, but offset successive frames so that they don't all open in the exact same spot
    (modify-frame-parameters
     frame
     '((user-position . t) (top . 0.5) (left . 0.5)))
    (set-frame-parameter frame 'top (+ (frame-parameter frame 'top) (* 10 frame-count)))
    (set-frame-parameter frame 'left (+ (frame-parameter frame 'left) (* 10 frame-count)))
    (select-frame-set-input-focus frame)

    ;; Start a new workspace. Seems redundant for a single file, but I don't want to pollute the existing workspaces.
    ;; Plus it means that cmd-w will actually close the current frame.
    (+workspace/new (file-name-nondirectory (directory-file-name dir)))

    ;; setup hook to delete workspace when the frame is closed
    (set-frame-parameter frame 'workspace (+workspace-current-name))

    (find-file initialfile)
    (when linenumber
      (goto-char (point-min))
      (forward-line (1- linenumber)))

    ;; Try and set up the directory as if it's a project-root (eg for SPC-s-p searching)
    (set-frame-parameter frame 'jds-frame-project-root dir)
    ))

(defun jds-projectile-project-root (orig-fun &rest args)
  (let ((frame-project (frame-parameter (selected-frame) 'jds-frame-project-root)))
    (or frame-project (apply orig-fun args))))
(advice-add 'projectile-project-root :around #'jds-projectile-project-root)
