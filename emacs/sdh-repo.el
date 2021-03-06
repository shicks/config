; Usage in emacs:
;   (face-remap-add-relative 'mode-line-inactive :background "#6e8b3d")
; Would be nice to set on a timer to periodically reset colors
;  - mode to ask for color "in background" that doesn't register as a use
;    buffer-list-update-hook & current-buffer should allow only ever
;    updating when changing buffers

(defun update-buffer-repo-color () (interactive))

(defvar sdh-update-buffer-repo-color-running nil)
(defvar sdh-repo-last-time 0)
(make-local-variable 'sdh-repo-last-time)

(defun update-buffer-repo-color ()
  (interactive)
  "Updates the modeline to indicate the current repo."
  (if (and (> (float-time) (+ sdh-repo-last-time 15))
           (not sdh-update-buffer-repo-color-running))
      (let* ((bf (buffer-file-name))
             (sdh-update-buffer-repo-color-running t)
             (maybe-dir (if bf (file-name-directory bf)))
             (dir (if (and maybe-dir (file-directory-p maybe-dir)) maybe-dir))
             (cmd (if dir (format "repoline colors --notouch --path=%s" dir)))
             (col (if cmd (shell-command-to-string cmd)))
             (spl (if col (split-string col ":")))
             (fg (if spl (car spl)))
             (bg (if spl (cadr spl))))
        (setq sdh-repo-last-time (float-time))
        (cond
         ((eq fg "#666666")
          ; TODO(sdh): consider adding additional colors to the list, so that each theme
          ; has four colors (fg:bg for regular and inverted?)
          (face-remap-add-relative 'mode-line-inactive
                                   :background "grey30" :foreground "grey80")
          (face-remap-add-relative 'mode-line
                                   :background "grey75" :foreground "black"))
         (fg
          ;; NOTE: This does not appear to work as intended - we end up with invisible
          ;; parts of the modeline.
          (face-remap-add-relative 'mode-line :background fg :foreground bg)
          (face-remap-add-relative 'mode-line-inactive :background bg :foreground "grey50")))))) ;"black"))))))

; TODO - other modeline faces that might want to change:
;   - vc-editied-state => currently hardcodes the background to grey75 - customize to none?
;     * also vc-up-to-date-state (w/ fg=black, which is also ugly)
;   - contrast is lower... we okay with that?

(add-hook 'buffer-list-update-hook 'update-buffer-repo-color)

(provide 'sdh-repo)
