;; -*- no-byte-compile: t -*-
;; Treat a tmux terminal similar to an rxvt.
(load "term/rxvt")

(defun terminal-init-tmux ()
  "Terminal initialization function for screen."
  ;; Use the xterm color initialization code.
  ;(terminal-init-rxvt)) ; TODO(sdh): sets light background for some reason
  (rxvt-register-default-colors)
  (tty-set-up-initial-frame-faces))
