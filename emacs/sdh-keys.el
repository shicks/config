;;; Main keybindings
;;

(require 'sdh-misc) ;; many of these functions are defined in sdh-misc

;; TODO(sdh): make a function that applies my own keybindings so that
;; I can simply write (sdh-kbd "C-M-,") instead of (kbd "ESC M-[ 36~,")

;; We would need to find an automated way to generate and maintain
;; all the .Xdefaults and ITerm2 config files.

;; Basic key Bindings
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "C-c k") 'kill-compilation)
(global-set-key (kbd "C-c ;") 'comment-region)
(global-set-key (kbd "C-c :") 'uncomment-region)
(global-set-key (kbd "C-c SPC") 'goto-line)
(global-set-key (kbd "C-c a") 'goto-char)
(global-set-key (kbd "C-c f") 'font-lock-fontify-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-c C-x r") 'revert-buffer)
(global-set-key (kbd "C-c M-q") 'unfill-paragraph)
(global-set-key (kbd "M-Y") 'yank-pop-back)
(global-set-key (kbd "C-x 4 o") 'swap-windows)
(global-set-key (kbd "C-x 4 p") 'swap-windows-back)
(global-set-key (kbd "M-D") 'delete-whitespace)
(global-set-key (kbd "C-<down>") 'my-scroll-up)
(global-set-key (kbd "C-<up>") 'my-scroll-down)
(global-set-key (kbd "M-<up>") 'move-line-or-region-up)
(global-set-key (kbd "M-<down>") 'move-line-or-region-down)
(global-set-key (kbd "C-c <tab>") 'indent-region)
(global-set-key (kbd "S-C-k") 'duplicate-line)
(global-set-key (kbd "S-C-t") 'transpose-chars-backwards)
(global-set-key (kbd "C-k") 'kill-line) ;; isn't this default?!?
(global-set-key (kbd "C-x <down>") 'bury-buffer)
(global-set-key (kbd "C-x M-b") 'other-buffer-other-window)
(global-set-key (kbd "C-x C-x") 'sdh-exchange-point-and-mark)
(global-set-key (kbd "C-x x") 'sdh-move-point-to-mark)
(global-set-key (kbd "C-x w") 'delete-region)
(global-set-key (kbd "C-a") 'sdh-beginning-of-line)
(global-set-key (sdh-kbd "C-x S-C-f") 'sdh-reopen-file-as-root)

(global-set-key (sdh-kbd "C-\\") 'toggle-input-method)
(global-set-key (sdh-kbd "C-,") 'sdh-previous-error)
(global-set-key (sdh-kbd "C-.") 'sdh-next-error)
;(global-set-key (sdh-kbd "C-M-,") 'smerge-keep-mine)
;(global-set-key (sdh-kbd "C-M-.") 'smerge-keep-other)
(global-unset-key (kbd "C-/"))
(global-set-key (sdh-kbd "C-/ C-,") 'smerge-keep-mine)
(global-set-key (sdh-kbd "C-/ C-.") 'smerge-keep-other)
(global-set-key (sdh-kbd "C-/ C-/") 'sdh-kill-middle-version)
;(global-set-key (sdh-kbd "C-/") 'sdh-next-error-new-file) ; never used this...
;(key-binding (sdh-kbd "C-/ M-[ 3 6 ~")) ; ???
;(global-set-key (sdh-kbd "C-/ ,") 'sdh-pick-top-version)
;(global-set-key (sdh-kbd "C-/ .") 'sdh-pick-bottom-version)

(global-set-key (sdh-kbd "C--") 'sdh-prev-window)
(global-set-key (sdh-kbd "C-=") 'sdh-other-window)
(global-set-key (sdh-kbd "C-+") 'mode-line-other-buffer)

;; visual-regexp
(if (sdh-try-require 'visual-regexp)
    (progn
      (global-set-key (kbd "C-c v m") 'vr/mc-mark)
      (global-set-key (kbd "C-c v q") 'vr/query-replace)
      (global-set-key (kbd "C-c v r") 'vr/replace)))

;;; phi-search works better with multiple-cursors
(if (sdh-try-require 'phi-search)
    (progn
      ; TODO(sdh): consider using C-/ C-s or C-? C-s
      (global-set-key (sdh-kbd "C-/ C-s") 'phi-search)
      (global-set-key (sdh-kbd "C-/ C-r") 'phi-search-backward)))

(if (sdh-try-require 'phi-replace)
    (progn
      (global-set-key (sdh-kbd "C-/ %") 'phi-replace)))

(if (sdh-try-require 'multiple-cursors)
    (progn
      ;; multiple-cursors mode
      (global-set-key (sdh-kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (sdh-kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (sdh-kbd "C-M-<") 'mc/skip-to-previous-like-this)
      (global-set-key (sdh-kbd "C-M->") 'mc/skip-to-next-like-this)
      (global-set-key (sdh-kbd "C-? C-?") 'mc/mark-all-like-this)
      ;(global-set-key (sdh-kbd "C-? C-<") 'mc/skip-to-previous-like-this)
      ;(global-set-key (sdh-kbd "C-? C->") 'mc/skip-to-next-like-this)
      ;(global-set-key (sdh-kbd "C-c C->") 'mc/mark-more-like-this-extended)
      ;(global-set-key (sdh-kbd "C-c C-<") 'mc/edit-lines)
      (if (sdh-try-require 'phi-search)
          (progn
            ;; multiple-cursors mode doesn't support isearch: use phi-search
            (define-key mc/keymap (kbd "C-s") 'phi-search)
            (define-key mc/keymap (kbd "C-r") 'phi-search-backward)
            (if (sdh-try-require 'phi-search-mc)
                (progn (phi-search-mc/setup-keys)
                       (add-hook 'isearch-mode-hook 'phi-search-from-isearch-mc/setup-keys)))))))


(global-set-key (kbd "C-x TAB") 'sdh-maybe-start-transient-indent-mode)
;; TODO - see if we actually like this?
;;(global-set-key (kbd "<backtab>") 'sdh-unindent-region)
(global-set-key (kbd "<backtab>") 'sdh-maybe-start-transient-indent-mode)


; C-x C-m C-m -> enable xterm-mouse-mode
(defun sdh-xterm-mouse-mode-t () "" (interactive) (xterm-mouse-mode t))
(global-set-key (kbd "C-x RET RET") 'sdh-xterm-mouse-mode-t)

;; Mouse wheel scrolling (this used to just work automatically...)
(global-set-key [mouse-4] 'sdh-mwheel-scroll)
(global-set-key [mouse-5] 'sdh-mwheel-scroll)


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
(defun set-prev-window-key ()
  ""
  (interactive)
  (global-set-key (kbd "C-x p") 'sdh-prev-window))
;; get rid of perforce bindings...
(global-set-key (kbd "C-x p C-x p") 'set-prev-window-key)
(global-set-key (kbd "C-x p") 'sdh-prev-window)
(global-set-key (kbd "C-x o") 'sdh-other-window)

(if (sdh-try-require 'window-jump)
    (progn
      (global-set-key (kbd "ESC M-[ a") 'window-jump-up)
      (global-set-key (kbd "ESC M-[ b") 'window-jump-down)
      (global-set-key (kbd "ESC M-[ c") 'window-jump-right)
      (global-set-key (kbd "ESC M-[ d") 'window-jump-left)))

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


;; rect-mark bindings
(define-key ctl-x-map (kbd "r C-@") 'rm-set-mark)
(define-key ctl-x-map (kbd "r C-SPC") 'rm-set-mark)
(define-key ctl-x-map (kbd "r C-x") 'rm-exchange-point-and-mark)
(define-key ctl-x-map (kbd "r C-w") 'rm-kill-region)
(define-key ctl-x-map (kbd "r M-w") 'rm-kill-ring-save)
(define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)

(autoload 'rm-set-mark "rect-mark"
  "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark"
  "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark"
  "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark"
  "Copy a rectangular region to the kill ring." t)
(autoload 'rm-mouse-drag-region "rect-mark"
  "Drag out a rectangular region with the mouse." t)



(provide 'sdh-keys)
