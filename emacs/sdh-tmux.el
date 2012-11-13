;;; Bindings peculiar to urxvt/tmux
;;
;; TODO(sdh): only enable these when $TERM == tmux?

;; delete and kp-delete
(global-set-key (kbd "<deletechar>") 'delete-char)
(global-set-key (kbd "M-O n") 'delete-char)

;; ctrl arrow keys
(global-set-key (kbd "M-[ A") 'my-scroll-down)
(global-set-key (kbd "M-[ B") 'my-scroll-up)
(global-set-key (kbd "M-[ C") 'forward-word)
(global-set-key (kbd "M-[ D") 'backward-word)

;; These are bound by URxvt
(global-set-key (kbd "M-[ 3 4 ~") 'duplicate-line)
(global-set-key (kbd "M-[ 3 5 ~") 'scroll-other-window-down)
(global-set-key (kbd "ESC <up>") 'move-line-or-region-up)
(global-set-key (kbd "ESC <down>") 'move-line-or-region-down)

(defun setup-tmux-comint-mode ()
  (interactive)
  (define-key comint-mode-map (kbd "M-[ A") 'comint-previous-input)
  (define-key comint-mode-map (kbd "M-[ B") 'comint-next-input))
(add-hook 'comint-mode-hook 'setup-tmux-comint-mode) ; this doesn't seem to work...?


(provide 'sdh-tmux)
