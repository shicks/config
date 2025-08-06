;;; Main keybindings
;;

(require 'sdh-misc) ;; many of these functions are defined in sdh-misc
(require 'sdh-kbd)

;; We would need to find an automated way to generate and maintain
;; all the .Xdefaults and ITerm2 config files.

;; Basic key Bindings
(defun sdh-nop () (interactive))
(global-set-key (kbd "ESC ESC ESC") 'sdh-nop) 
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
(global-set-key (kbd "C-x M-d") 'sdh-delete-word)
(global-set-key (kbd "C-a") 'sdh-beginning-of-line)
(global-set-key (kbd "C-x S-C-f") 'sdh-find-file-as-root)
(global-set-key (kbd "C-x S-C-r") 'sdh-reopen-file-as-root)

(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key (kbd "C-,") 'sdh-previous-error)
(global-set-key (kbd "C-.") 'sdh-next-error)
;(sdh-global-set-key "C-M-," 'smerge-keep-mine)
;(sdh-global-set-key "C-M-." 'smerge-keep-other)
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-/ C-,") 'smerge-keep-upper)
(global-set-key (kbd "C-/ C-.") 'smerge-keep-lower)
(global-set-key (kbd "C-/ C-/") 'smerge-keep-all)
;(global-set-key (kbd "C-/ C-/") 'sdh-kill-middle-version)
(global-set-key (kbd "C-/ C-x") 'smerge-refine)
;(sdh-global-set-key "C-/" 'sdh-next-error-new-file) ; never used this...
;(key-binding "C-/ M-[ 3 6 ~") ; ???
;(sdh-global-set-key "C-/ ," 'sdh-pick-top-version)
;(sdh-global-set-key "C-/ ." 'sdh-pick-bottom-version)

(global-set-key (kbd "C-S-e") 'kmacro-end-and-call-macro)

(global-set-key (kbd "C--") 'sdh-prev-window)
(global-set-key (kbd "C-=") 'sdh-other-window)
;(sdh-global-set-key "C-+" 'mode-line-other-buffer)  ;; too distracting
(global-set-key (kbd "C-9") 'mode-line-other-buffer)  ;; too distracting

; Stolen from vim
(global-set-key (kbd "C-5") 'forward-or-backward-sexp)

; suspend-frame is bad: open up C-z as a prefix and bind C-zC-z to my
; own version of it.
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z C-z") 'sdh-suspend-frame)



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
      (global-set-key (kbd "C-/ C-s") 'phi-search)
      (global-set-key (kbd "C-/ C-r") 'phi-search-backward)))

(if (sdh-try-require 'phi-replace)
    (progn
      (global-set-key (kbd "C-/ %") 'phi-replace)))

(if (sdh-try-require 'multiple-cursors)
    (progn
      ;; multiple-cursors mode
      (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
      (global-set-key (kbd "C->") 'mc/mark-next-like-this)
      (global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
      (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
      (global-unset-key (kbd "C-?")) ; not sure what this is by default?
      (global-set-key (kbd "C-? C-?") 'mc/mark-all-like-this)
      ;(sdh-global-set-key "C-? C-<" 'mc/skip-to-previous-like-this)
      ;(sdh-global-set-key "C-? C->" 'mc/skip-to-next-like-this)
      ;(sdh-global-set-key "C-c C->" 'mc/mark-more-like-this-extended)
      ;(sdh-global-set-key "C-c C-<" 'mc/edit-lines)
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
(defun sdh-xterm-mouse-mode-t () "" (interactive) (xterm-mouse-mode t) (sdh-kbd-init))
(global-set-key (kbd "C-x RET RET") 'sdh-xterm-mouse-mode-t)
(sdh-xterm-mouse-mode-t)

;; C-j is now emitted by the Enter key since C-m was doing weird things
;; in the terminal.  I never want electric indent, so change this from
;; electric-newline-and-maybe-indent to just plain old newline.
;; NOTE: the underlying issue was `stty -icrnl`: by enabling icrnl
;;       (with `stty icrnl`) the ^M issue is no longer a problem.
;; (global-set-key (kbd "C-j") 'newline)

;; Mouse wheel scrolling (this used to just work automatically...)
(global-set-key [mouse-4] 'sdh-mwheel-scroll)
(global-set-key [mouse-5] 'sdh-mwheel-scroll)
(global-set-key (kbd "<mode-line> <mouse-4>") 'previous-buffer)
(global-set-key (kbd "<mode-line> <mouse-5>") 'next-buffer)


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
(ignore-errors
  (global-set-key (kbd "C-x p C-x p") 'set-prev-window-key))
(global-set-key (kbd "C-x p") 'sdh-prev-window)
(global-set-key (kbd "C-x o") 'sdh-other-window)

;; TODO(sdh): alacritty now sends these sequences and they're not binding
;; normally for some reason?
(global-set-key (kbd "M-[ H") 'move-beginning-of-line)
(global-set-key (kbd "M-[ F") 'move-end-of-line)

(if (or (sdh-try-require 'window-jump) (fboundp 'window-jump-up))
    (progn
      (global-set-key (kbd "C-M-<up>") 'window-jump-up)
      (global-set-key (kbd "C-M-<down>") 'window-jump-down)
      (global-set-key (kbd "C-M-<left>") 'window-jump-left)
      (global-set-key (kbd "C-M-<right>") 'window-jump-right)
      ; TODO(sdh): remove these keybindings once old tmux (and iterm?) no longer sends them?
      (global-set-key (kbd "ESC M-[ a") 'window-jump-up)
      (global-set-key (kbd "ESC M-[ b") 'window-jump-down)
      (global-set-key (kbd "ESC M-[ c") 'window-jump-right)
      (global-set-key (kbd "ESC M-[ d") 'window-jump-left)
      ; TODO(sdh): these are new iterm bindings...?
      (global-set-key (kbd "ESC M-O A") 'window-jump-up)
      (global-set-key (kbd "ESC M-O B") 'window-jump-down)
      (global-set-key (kbd "ESC M-O C") 'window-jump-right)
      (global-set-key (kbd "ESC M-O D") 'window-jump-left)
))

(if (sdh-try-require 'doremi-cmd)
    (progn
      (global-set-key (kbd "C-c t w") 'doremi-window-height+)
      ))

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
;; copied (and modified) from https://www.emacswiki.org/emacs/RectangleMark
(define-key ctl-x-map (kbd "r C-@") 'rm-set-mark) ; note: C-@ is C-SPC in terminal
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


;; Mac rebinds home/end to start/end of file, which is maddening.
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "<home>") 'beginning-of-line)

;; Mark/region helpers - C-x SPC
;; Something previously bound [C-x SPC] to rectangle-mark-mode.
;; I thought it was me, but I can't find it, so just unset it here.
(global-unset-key (kbd "C-x SPC"))
(global-set-key (kbd "C-x SPC r") 'rectangle-mark-mode)
(defun sdh-activate-mark () (interactive) (activate-mark))
(global-set-key (kbd "C-x SPC a") 'sdh-activate-mark)
;(global-set-key (kbd "C-x C-a") 'sdh-activate-mark)



;; TOGGLES - C-c t
; l: line-move-visual to jump over wrapped lines.
(global-set-key (kbd "C-c t l") 'sdh-toggle-line-move-visual)


;;;;;;;;;
;; TODO - replace these with better sdh-set-key - for now this is from macbook
(global-set-key (kbd "M-[ 1 ; 5 D") 'backward-word)  ; C-left
(global-set-key (kbd "M-[ 1 ; 5 C") 'forward-word)  ; C-right
(global-set-key (kbd "M-[ 1 ; 5 A") 'my-scroll-down)  ; C-up
(global-set-key (kbd "M-[ 1 ; 5 B") 'my-scroll-up)  ; C-down
(global-set-key (kbd "M-[ 1 ; 3 A") 'move-line-or-region-up)  ; M-up
(global-set-key (kbd "M-[ 1 ; 3 B") 'move-line-or-region-down)  ; M-down
(global-set-key (kbd "M-[ 1 ; 2 A") 'sdh-previous-line-visual)  ; S-up
(global-set-key (kbd "M-[ 1 ; 2 B") 'sdh-next-line-visual)  ; S-down
(global-set-key (kbd "M-[ 1 ~") 'sdh-beginning-of-line)  ; home
(global-set-key (kbd "<select>") 'end-of-line)  ; end  (M-[ 4 ~)

(global-set-key (kbd "M-[ 4 ~") 'end-of-line)  ; end (on linux?)

;;;;;;;
;; org-mode S-M-arrow bindings
;; TODO - would be nice to not just deal with tables...? how to delegate to other binding?
;; TODO - would be nice to not have to actually *load* org-mode and instead just add a hook
;;        to run when it autoloads
(if (sdh-try-require 'org)
    (progn
      ;(define-key org-mode-map (kbd "M-[ 1 ; 3 a") 'org-table-move-row-up) ; M-up
      ;(define-key org-mode-map (kbd "M-[ 1 ; 3 b") 'org-table-move-row-down) ; M-down
      (define-key org-mode-map (kbd "M-[ 1 ; 3 d") 'org-table-move-column-left) ; M-left
      (define-key org-mode-map (kbd "M-[ 1 ; 3 c") 'org-table-move-column-right) ; M-right
      (define-key org-mode-map (kbd "M-[ 1 ; 4 a") 'org-table-delete-row) ; S-M-up
      (define-key org-mode-map (kbd "M-[ 1 ; 4 b") 'org-table-insert-row) ; S-M-down
      (define-key org-mode-map (kbd "M-[ 1 ; 4 d") 'org-table-delete-column) ; S-M-left
      (define-key org-mode-map (kbd "M-[ 1 ; 4 c") 'org-table-insert-column) ; S-M-right
      ))

(provide 'sdh-keys)
