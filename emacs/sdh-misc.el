;; Miscellaneous settings, until I find a better place for them

;;;;;;;;;;;;;;;;
;; From redhat

(setq require-final-newline t)  ; always end a file with a newline

;;;;;;;;;;;;;;;;
;; Misc global config
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(column-number-mode 1)  ; Show column numbers
(global-font-lock-mode 1)  ; Syntax highlighting
(setq default-input-method "TeX")  ; default to TeX input on C-\

(setq-default indent-tabs-mode nil)  ; Never use tabs to indent
(add-hook 'emacs-lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq inhibit-splash-screen t)  ; Never show splash screen

(setq delete-active-region nil) ; don't delete active regions on backspace

;(setq debugger 'edebug-debug)  ; Use edebug for emacs lisp

(require 'savehist) ;; this was history.el but it led to circular dep
(savehist-mode 1)


;;;;;;;;;;;;;;;;
;; Misc utility functions

(defun string-repeat (str n)
  (let ((retval ""))
    (dotimes (i n)
      (setq retval (concat retval str)))
    retval))

(defun space-p (x)
  "Returns true if the input char is a space"
  (member x '(10 13 32)))


;;;;;;;;;;;;;;;;
;; Clipboard handling

;; TODO(sdh): figure this out!

(defun paste-primary-at-point ()
  "Pastes from the primary X selection at point."
  (interactive)
  (let ((bs (buffer-size)))
    (shell-command "xclip -o" 1)
    (forward-char (- (buffer-size) bs))))

;; TODO(sdh): find a prefix we can send for ALL super/C-S- keys in urxvt
;; We want one that will be minimally intrusive, like F-13 or something
;; We could also just teach zsh/bash to ignore the prefix and either
;; just handle the S-k or the C-k... (probably the latter).

(defun copy-to-x ()
  "Copies the current region to the X clipboard via the urxvt mycopy extension."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point) (mark)))
         (encoded (base64-encode-string text t))
         (directive (concat "\033]777;mycopy;" encoded "\007")))
    ; (shell-command-on-region (point) (mark) "xclip -in"))
    ; (message encoded)
    ;; non-tmux:
    ; (send-string-to-terminal (concat "\C-[]777;mycopy;" encoded "\C-G"))
    ;; tmux (post update) -> double the \033's and end with \033\\
    (if (fboundp 'send-string-through-tmux)
        (send-string-through-tmux directive)
      (send-string-to-terminal directive))
    ;;(send-string-to-terminal (concat "\C-[Ptmux;\C-[\C-[]777;mycopy;" encoded "\C-G\C-[\\"))
))

;;;;;;;;;;;;;;;;
;; From droundy

(defun my-scroll-up (num)
  "Scroll text up ARG lines keeping cursor at same screen position"
  (interactive "p")
  (scroll-up num)
  (next-line num))

(defun my-scroll-down (num)
  "Scroll text down ARG lines keeping cursor at same screen position"
  (interactive "p")
  (scroll-down num)
  (previous-line num))


;;;;;;;;;;;;;;;;
;; Moves a line or region up or down

(defun move-line-or-region (n)
  "Move the current line/region up/down by N lines."
  (interactive "p")
  (let* ((deactivate-mark nil)
         (mark-index (- (if (mark) (mark) 0) (point)))
         (start (if mark-active (min (point) (mark)) (point-at-bol)))
         (end (if mark-active (max (point) (mark)) (+ 1 (point-at-eol))))
         (active mark-active))
    (goto-char (move-region start end n))
    (if active (set-mark (+ (point) mark-index)))))

(defun move-region (start end n)
  "Move the region between start and end down n lines."
  (interactive "p")
  (let* ((start-index (- (point) start))
         (content (delete-and-extract-region start end))
         (start-col (progn (goto-char start) (current-column)))
         (target-line (progn (goto-char start) (forward-line n) (point-at-bol)))
         (rest-of-target (buffer-substring target-line
                          (progn (goto-char target-line) (point-at-eol))))
         (missing-spaces (string-repeat " " (max 0 (- start-col (length rest-of-target))))))
    ;; first trim any extra added spaces
    ;; NOTE: problem - the delete-region throws off the other counts!
    ;;       -> need to use markers instead of indexes
    ;(if (and (= ?\n (char-after start)) (/= ?\n (char-before start)))
    ;    (save-excursion
    ;      (goto-char start)
    ;      (re-search-backward "[^[:space:]]" (point-at-bol) 1)
    ;      (delete-region (point) start)))
    ;; now add any spaces we need at the end of target line
    (goto-char target-line)
    (end-of-line)
    (insert missing-spaces)
    ;; now insert the string
    (goto-char (+ target-line start-col))
    (insert content)
    ;; restore point to the correct place within the content
    (+ target-line start-col start-index)))

(defun move-line-or-region-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line-or-region (if (null n) -1 (- n))))

(defun move-line-or-region-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line-or-region (if (null n) 1 n)))


;;;;;;;;;;;;;;;;
;; Kill ring

(defun yank-pop-back (num)
  "Goes backwards in kill ring"
  (interactive "p")
  (yank-pop -1))


;;;;;;;;;;;;;;;;
;; Chmod

(defun make-executable ()
  (interactive)
  (shell-command (format "chmod +x %s" (buffer-file-name))))


;;;;;;;;;;;;;;;;
;; Text handling

(defun unfill-paragraph ()
  "Unfills a paragraph. converting it back to a single line."
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(defun delete-whitespace ()
  "Deletes all whitespace around point"
  (interactive)
  (while (space-p (char-before)) (backward-delete-char 1))
  (while (space-p (char-after)) (delete-char 1))
  (insert " ")
)

;; The extra (b-o-l) shouldn't be needed but there's a weird bug
;; by doing (duplicate-line) (previous-line) (duplicate-line) that
;; appears to occur because (next-line) in the macro is putting
;; us in the middle of the line rather than at the beginning.
;; --- note: this is now obsolete...?
(defun duplicate-line ()
  "Duplicates a line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)) (line))
      ;(next-line) (beginning-of-line) ;; why is this needed?
      (end-of-line)
      (setq line (buffer-substring beg (point)))
      (insert (concat "\n" line))))
  (next-line))


;;;;;;;;;;;;;;;;
;; Visited/killed file history

(defadvice kill-buffer (before save-killed-buffer (arg))
  "Saves the file visited by the most-recently-killed buffer."
  (let ((file (buffer-file-name (get-buffer arg))))
    (if (stringp file)
        (progn  ;; TODO(sdh): at some point we should just use file-name-history
          (set-variable 'file-name-history (cons file file-name-history))))))

(ad-activate 'kill-buffer)
(set-variable 'history-length 1000)


;;;;;;;;;;;;;;;;
;; UI elements

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;;;;;;;;;;;;;;;
;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;;;;;;;;;;;;;;;;
;; Line numbers

(defun add-line-numbers-to-region (start end)
  (interactive "r")
  (let* ((lines (count-lines start end))
         (fmt (if (< lines 100) " %02d " " %03d ")))
    (add-line-numbers-with-format start lines fmt)))

(defun add-line-numbers-to-region-with-pattern (start end fmt)
  (interactive (let ((string (read-string
                              "Format string ( %02d ): " nil nil " %02d ")))
                 (list (region-beginning) (region-end) string)))
  (add-line-numbers-with-format start (count-lines start end) fmt))

(defun add-line-numbers-with-format (start lines fmt)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (dotimes (line lines)
      (insert (format fmt (+ 1 line)))
      (goto-char (point-at-bol 2)))))

;;;;;;;;;;;;;;;;
;; Editing functions

(defun transpose-chars-backwards (arg)
  (interactive "p")
  (transpose-chars (- arg)))

;;;;;;;;;;;;;;;;
;; Window management

(defun swap-windows (win)
  "Swap the buffers in two windows"
  (interactive "p")
  (let ((my-this-buffer (buffer-name)))
    (other-window win)
    (let ((my-other-buffer (buffer-name)))
      (switch-to-buffer my-this-buffer)
      (other-window (- win))
      (switch-to-buffer my-other-buffer))))

(defun swap-windows-back (win)
  "Swap the buffers in two windows"
  (interactive "p")
  (let ((my-this-buffer (buffer-name)))
    (prev-window win)
    (let ((my-other-buffer (buffer-name)))
      (switch-to-buffer my-this-buffer)
      (prev-window (- win))
      (switch-to-buffer my-other-buffer))))

(defun revert-buffer-list (buffers)
  "Reverts all the buffers in the passed list"
  (let ((current-buffer (buffer-name)))
    (switch-to-buffer (buffer-name (car buffers)))
    (if (buffer-file-name) (revert-buffer))
    (switch-to-buffer (current-buffer)))
  (if (cdr buffers) (revert-buffer-list (cdr buffers))))
(defun revert-all-buffers ()
  "Reverts all buffers that point to files"
  (revert-buffer-list (buffer-list)))

;; prev-window maybe from droundy?
(defun prev-window (win)
  "Move to previous window"
  (interactive "p")
  (other-window (- win)))

;; For use in slightly-cramped screens
;; We could consider making the size variable, depending on file type?
(defun enlarge-window-to-100 ()
  "Enlarges the given window to 100 columns under certain circumstances."
  (interactive)
  (if (and (not (one-window-p))
           (< (frame-width) 203)
           (> (frame-width) 163)
           (< (window-width) 101))
      (enlarge-window-horizontally (- 101 (window-width)))))

(defun sdh-other-window (win)
  "Move to next window and maybe enlarge it"
  (interactive "p")
  (other-window win)
  (enlarge-window-to-100))

(defun sdh-prev-window (win)
  "Move to prev window and maybe enlarge it"
  (interactive "p")
  (prev-window win)
  (enlarge-window-to-100))

;; Many functions replace the buffer in the current window when
;; we'd rather them open in a new window.  This function moves
;; the current buffer to the other window and then opens the
;; previous buffer in this window.
(defun other-buffer-other-window ()
  "Moves this buffer to other window and other buffer in this window."
  (interactive)
  (switch-to-buffer (other-buffer))
  (sdh-other-window 1)
  (switch-to-buffer (other-buffer)))

;; exchange-point-and-mark has weird behavior in transient-mark-mode.  This
;; simply reverses the effect of the prefix argument under certain situations.
(defun sdh-exchange-point-and-mark (arg)
  "Exchange point and mark.  Same as `exchange-point-and-mark', but we invert
the prefix argument in transient mark mode (unless the mark is active)."
  (interactive "P")
  (exchange-point-and-mark
   (if (and transient-mark-mode (not mark-active)) (not arg) arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'sdh-misc)
