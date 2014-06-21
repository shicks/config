;;; Main keybindings
;;

(require 'sdh-misc) ;; many of these functions are defined in sdh-misc

;; Basic key Bindings
(global-set-key "\C-Cc" 'compile)
(global-set-key "\C-Cr" 'recompile)
(global-set-key "\C-Ck" 'kill-compilation)
(global-set-key "\C-C;" 'comment-region)
(global-set-key "\C-C:" 'uncomment-region)
(global-set-key "\C-C " 'goto-line)
(global-set-key "\C-Ca" 'goto-char)
(global-set-key "\C-Cf" 'font-lock-fontify-buffer)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)
(global-set-key "\C-c\C-xr" 'revert-buffer)
(global-set-key "\C-c\M-q" 'unfill-paragraph)
(global-set-key "\M-Y" 'yank-pop-back)
(global-set-key "\C-x4o" 'swap-windows)
(global-set-key "\C-x4p" 'swap-windows-back)
(global-set-key "\M-D" 'delete-whitespace)
(global-set-key [C-down] 'my-scroll-up)
(global-set-key [C-up] 'my-scroll-down)
(global-set-key (kbd "M-<up>") 'move-line-or-region-up)
(global-set-key (kbd "M-<down>") 'move-line-or-region-down)
(global-set-key (kbd "C-c <tab>") 'indent-region)
(global-set-key (kbd "S-C-k") 'duplicate-line)
(global-set-key (kbd "S-C-t") 'transpose-chars-backwards)
(global-set-key "\C-k" 'kill-line)
(global-set-key '[?\C-x down] 'bury-buffer)
(global-set-key (kbd "C-x M-b") 'other-buffer-other-window)
(global-set-key (kbd "C-x C-x") 'sdh-exchange-point-and-mark)
(global-set-key (kbd "C-x x") 'sdh-move-point-to-mark)

;; TODO(sdh): C-M-y and C-M-c should call xclip (or use urxvt mycopy)
;; (global-set-key (kbd "C-M-y") 'clipboard-yank)
;; (global-set-key (kbd "<mouse-2>") 'paste-primary-at-point)

;; Work more nicely with subword-mode
(global-set-key (kbd "C-<left>") 'backward-word)
(global-set-key (kbd "C-<right>") 'forward-word)

;; This used to be automatic, but something changed.
(global-set-key [(control backspace)] 'backward-kill-word)
(global-set-key [(super backspace)] 'backward-kill-word)



;; perforce keybindings are a pain, and seem to keep overriding me.
(defun set-prev-window-key () "" (interactive) (global-set-key "\C-xp" 'sdh-prev-window))
(global-set-key "\C-xp\C-xp" 'set-prev-window-key)
(global-set-key "\C-xp" 'sdh-prev-window)
(global-set-key "\C-xo" 'sdh-other-window)

;; Obsolete bindings
;(global-set-key "\C-CC-Z" '(replace-regexp "<[^<>]*>" "")) ;; For c++ errors
;(global-set-key "\C-x5a" 'beginning-of-buffer)
;(global-set-key "\C-x5e" 'end-of-buffer)


;; TODO(sdh): find a better way to handle this...
;(defun sdh-subword-mode (map)
;  "Manually sets up subword mode for a given mode map (i.e. js2-mode-map)"
;  (define-key map (kbd "C-<left>") 'subword-backward)
;  (define-key map (kbd "C-<right>") 'subword-forward)
;  (global-set-key (kbd "M-[ C") 'subword-forward)
;  (global-set-key (kbd "M-[ D") 'subword-backward)
;  (define-key map (kbd "M-@") 'subword-mark)
;  (define-key map (kbd "C-<delete>") 'subword-kill)
;  (define-key map (kbd "M-d") 'subword-kill)
;  (define-key map (kbd "C-<backspace>") 'subword-backward-kill)
;  (define-key map (kbd "M-<delete>") 'subword-backward-kill)
;  (define-key map (kbd "M-t") 'subword-transpose)
;  (define-key map (kbd "M-c") 'subword-capitalize)
;  (define-key map (kbd "M-u") 'subword-upcase)
;  (define-key map (kbd "M-l") 'subword-downcase))


(provide 'sdh-keys)
