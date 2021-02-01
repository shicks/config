;; Miscellaneous settings, until I find a better place for them

;;;;;;;;;;;;;;;;
;; From redhat

(setq require-final-newline t)  ; always end a file with a newline

;;;;;;;;;;;;;;;;
;; Use MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

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
(setq set-mark-command-repeat-pop t) ; C-u C-SPC C-SPC... will cycle

;(setq debugger 'edebug-debug)  ; Use edebug for emacs lisp

(require 'savehist) ;; this was history.el but it led to circular dep
(setq savehist-file "~/.emacs_history")
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

(defun sdh-find-file-as-root (file)
  "Finds a file and uses tramp to open it as root"
  (interactive "FFind file (root): ")
  (find-file (concat "/sudo:root@localhost:" file)))

(defun sdh-reopen-file-as-root ()
  "Reopens the current file with tramp to open it as root"
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name))))

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
    ; figure out what's at end of both lines - is it end of buffer at either?
    ;(describe-char end)
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
    ;; TODO(sdh): either line is missing a newline at end, then add it


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
  (if arg
      (let ((file (buffer-file-name (get-buffer arg))))
        (if (stringp file)
            (progn  ;; TODO(sdh): at some point we should just use file-name-history
              (set-variable 'file-name-history (cons file file-name-history))
              (sdh-filter-file-name-history))))))

(defadvice save-buffer (after remove-boring-files-from-history)
  "Removes certain files from file-name-history."
  (sdh-filter-file-name-history))

(defvar sdh-file-name-filter '()
  "Regexes of file names to exclude from history.")

(defun sdh-filter-file-name-history ()
  "Filters file name history."
  (if (and file-name-history
           ;; TODO(sdh): consider checking the first N>1 files
           (sdh-filter-individual-file (car file-name-history)))
      (progn
        (message "Filtering file from history: %s" (car file-name-history))
        (set-variable 'file-name-history (cdr file-name-history)))))

(defun sdh-filter-individual-file (file)
  "Returns t if any of the file name filters matches."
  (delq nil (mapcar (lambda (r) (string-match r file)) sdh-file-name-filter)))

(ad-activate 'kill-buffer)
(ad-activate 'save-buffer)
(set-variable 'history-length 1000)

;;;;;;;;;;;;;;;;
;; UI elements

(defmacro sdh-call-if-bound (&rest forms)
  "Calls the top-level forms only if their functions are bound."
  (cons 'progn (mapcar (lambda (form) `(if (fboundp (car ',form)) ,form)) forms)))

(sdh-call-if-bound
 (menu-bar-mode -1)
 (tool-bar-mode -1)
 (scroll-bar-mode -1))


;;;;;;;;;;;;;;;;
;; http://stackoverflow.com/questions/1242352/get-font-face-under-cursor-in-emacs

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
(global-set-key (kbd "C-h C-f") 'what-face)


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

(defvar sdh-dynamic-frame-width nil ; 100
  "*The target width for frames.")

;; For use in slightly-cramped screens
;; We could consider making the size variable, depending on file type?
(defun enlarge-window-to-100 ()
  "Enlarges the given window to 100 columns under certain circumstances."
  (interactive)
  (if sdh-dynamic-frame-width
      (let* ((upper (+ 3 (* 2 sdh-dynamic-frame-width)))
             (lower (+ 3 (* 2 (- sdh-dynamic-frame-width 20))))
             (target (+ 1 sdh-dynamic-frame-width)))
        (if (and (not (one-window-p))
                 (< (frame-width) upper)
                 (> (frame-width) lower)
                 (< (window-width) target))
            (enlarge-window-horizontally (- target (window-width)))))))

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

(defun sdh-move-point-to-mark ()
  "Moves point to mark"
  (interactive)
  (set-window-point nil (mark)))

;; wraps a (require) in a catch block.
(defun sdh-try-require (arg)
  "Tries to require a file, returns t if successful, nil otherwise"
  (condition-case nil
      (require arg)
    (error (message "WARNING: Could not require %s" arg)
           nil)))

;;;;;;;;;;;;;;;;
;; Backup files

(setq backup-directory-alist `(("." . "~/.emacs_backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defun sdh-delete-old-backup-files ()
  "Deletes old backup files."
  (interactive)
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
        (current (float-time (current-time))))
    (dolist (file (directory-files "~/.emacs_backups" t))
      (when (and (backup-file-name-p file)
                 (> (- current (float-time (nth 5 (file-attributes file))))
                    week))
        (message "%s" file)
        (delete-file file))))
)
(sdh-delete-old-backup-files)


;;; 2-character tabs in go mode
(defun sdh-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save)
  (setq tab-width 2 indent-tabs-mode 1))
(add-hook 'go-mode-hook 'sdh-go-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'."
  (interactive)
  (let (p1 p2)
    (if (region-active-p)
        (progn (setq p1 (region-beginning))
               (setq p2 (region-end)))
      (progn (setq p1 (line-beginning-position))
             (setq p2 (line-end-position))))
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: 「%s」." (buffer-substring-no-properties p1 p2))))
(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'."
  (interactive)
  ;(when (use-region-p)   ;; Don't bother with this
  ;  (delete-region (region-beginning) (region-end) )
  ;  )
  (insert-register ?1 t))

(defun sdh-save-position-to-register-1 ()
  "Save the current cursor position to register 1."
  (interactive)
  (set-register ?1 (point-marker)))
(defun sdh-restore-position-from-register-1 ()
  "Restore the cursor position from register 1."
  (interactive)
  (goto-char (get-register ?1)))

(defun sdh-copy-or-insert-register (arg)
  "Copies or inserts the contents of the '1' register: basically C-x r [si] 1"
  (interactive "P")
  (if arg (sdh-save-x-to-register-1) (sdh-restore-x-from-register-1)))

(defun sdh-save-x-to-register-1 ()
  (interactive)
  (if (and transient-mark-mode mark-active)
      (xah-copy-to-register-1)
    (sdh-save-position-to-register-1)))
(defun sdh-restore-x-from-register-1 ()
  (interactive)
  (if (markerp (get-register ?1))
      (sdh-restore-position-from-register-1)
    (xah-paste-from-register-1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation buffer navigation

(defun next-error-new-file ()
  (interactive)
  (with-current-buffer next-error-last-buffer
    (compilation-next-file 1)
    (compile-goto-error)))

(defun sdh-is-flymake ()
  (interactive)
  (and (bound-and-true-p flymake-mode) (not (get-buffer-window "*compilation*"))))

(defun sdh-is-flycheck ()
  (interactive)
  (and (bound-and-true-p flycheck-mode)))

(defun sdh-next-error-new-file () (interactive) (next-error-new-file))

(defun sdh-next-error () (interactive)
  (cond
   ((get-buffer "*compilation*") (next-error))
   ((sdh-is-flymake) (flymake-goto-next-error))
   ((sdh-is-flycheck) (flycheck-next-error))
   (t (next-error))))

(defun sdh-previous-error () (interactive)
  (cond
   ((get-buffer "*compilation*") (previous-error))
   ((sdh-is-flymake) (flymake-goto-prev-error))
   ((sdh-is-flycheck) (flycheck-previous-error))
   (t (previous-error))))

;; Also turn on hl-line-mode and add a hook to make sure it runs

(defun sdh-compilation-mode-hook () (interactive)
  (hl-line-mode 1))
(add-hook 'compilation-mode-hook 'sdh-compilation-mode-hook)

(defun sdh-next-error-hook () (interactive)
  (with-current-buffer next-error-last-buffer (when hl-line-mode (hl-line-highlight))))
(add-hook 'next-error-hook 'sdh-next-error-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conflict resolution

; NOTE: use smerge-keep-mine
(defun sdh-pick-top-version (&optional arg)
  "Run from the '========' line of a marked conflict.  Deletes the >>> version and keeps the <<< version."
  (interactive "p")
  (kmacro-exec-ring-item (quote (" >>>>>>>OC<<<<<<<" 0 "%d")) arg))

; NOTE: use smerge-keep-other
(defun sdh-pick-bottom-version (&optional arg)
  "Run from the '========' line of a marked conflict.  Deletes the <<< version and keeps the >>> version."
  (interactive "p")
  (kmacro-exec-ring-item (quote ("OB <<<<<<<>>>>>>>" 0 "%d")) arg))

; NOTE: use smerge-keep-other
(defun sdh-kill-middle-version (&optional arg)
  "Run from the '========' line of a marked conflict.  Deletes the <<< version and keeps the >>> version."
  (interactive "p")
  (kmacro-exec-ring-item (quote ("<<<<<<<|||||||OB =======" 0 "%d")) arg))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line handling

(defun sdh-beginning-of-line ()
  "Modification of (beginning-of-line) that jumps back and forth between the indent and the actual beginning-of-line."
  (interactive)
  (if (and (eq last-command 'sdh-beginning-of-line)
           (= (point-at-bol) (point)))
      (progn (end-of-line) (back-to-indentation))
    (beginning-of-line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indenting

;; TODO - these are only really useful until 24.4 when 'C-x TAB'
;; is updated to use a transient mode, allowing left/right to
;; "just work".

(defun sdh-indent-rigidly (n)
  "Indent region or otherwise current line"
  (let* ((use-region (and transient-mark-mode mark-active))
         (rstart (if use-region (region-beginning) (point-at-bol)))
         (rend   (if use-region (region-end)       (point-at-eol)))
         (deactivate-mark "irrelevant")) ; avoid deactivating mark
    (indent-rigidly rstart rend n)))

(defun sdh-rigid-right (num)
  "Indents the region rigidly by 1 character [bound to C-)]"
  (interactive "P")
  (sdh-indent-rigidly (prefix-numeric-value num)))

(defun sdh-rigid-left (num)
  "Indents the region rigidly by -1 character [bound to C-(]"
  (interactive "P")
  (sdh-indent-rigidly (- (prefix-numeric-value num))))

(defun sdh-unindent-region ()
  "Unindents by two."
  (interactive)
  (sdh-rigid-left 2))

;; Transient indent mode: C-x TAB, then < or >
(define-minor-mode sdh-transient-indent-mode
  "Transiently indents a region"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<") 'sdh-rigid-left)
            (define-key map (kbd "[") 'sdh-rigid-left)
            (define-key map (kbd "<backtab>") 'sdh-rigid-left)
            (define-key map (kbd "<left>") 'sdh-rigid-left)
            (define-key map (kbd ">") 'sdh-rigid-right)
            (define-key map (kbd "]") 'sdh-rigid-right)
            (define-key map (kbd "TAB") 'sdh-rigid-right)
            (define-key map (kbd "<right>") 'sdh-rigid-right)
            map)
  (if sdh-transient-indent-mode
      (add-hook 'pre-command-hook 'sdh-transient-indent-mode-quit)))
(defun sdh-transient-indent-mode-quit ()
  "Quits transient indent mode unless this-command is sdh-rigid-*"
  (interactive)
  (when (not (member this-command '(sdh-rigid-left
                                    sdh-rigid-right
                                    universal-argument
                                    universal-argument-other-key
                                    universal-argument-more
                                    digit-argument
                                    negative-argument)))
    (remove-hook 'pre-command-hook 'sdh-transient-indent-mode-quit)
    (sdh-transient-indent-mode -1)))
(defun sdh-maybe-start-transient-indent-mode (arg)
  "Starts transient indent mode unless there's a prefix argument, in which case it falls back on simply indent-rigidly."
  (interactive "P")
  (if arg
      (sdh-indent-rigidly (prefix-numeric-value arg))
    (sdh-transient-indent-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse wheel handling (this used to "just work")

(defun sdh-event-window (event)
  (posn-window (event-start event)))
(defun sdh-event-button (event)
  (let ((x (symbol-name (event-basic-type event))))
    (if (not (string-match "^mouse-\\([0-9]+\\)" x))
        (error "Not a button event: %S" event))
    ;; NOTE: used to be string-to-int ...?
    (string-to-number (substring x (match-beginning 1) (match-end 1)))))

(defun sdh-mwheel-scroll (event)
  (interactive "e")
  (let ((curwin (prog1 (selected-window) (select-window (sdh-event-window event))))
        (button (sdh-event-button event)))
    (unwind-protect
        (cond ((= button 4) (scroll-down 10))
              ((= button 5) (scroll-up 10))
              (t (error "Bad binding")))
      (if curwin (select-window curwin)))))


(defun sdh-delete-word ()
  (interactive)
  (save-excursion
    (let* ((start (point))
           (end (progn (forward-word) (point))))
      (delete-region start end))))

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;; Highlight current line, only when idle.
(if (sdh-try-require 'hl-line+)
    (toggle-hl-line-when-idle))

; Use bracketed paste if it's installed (doesn't seem to work...)
;(if (sdh-try-require 'bracketed-paste)
;    (bracketed-paste-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some random functions from the internet

;; From https://emacs.stackexchange.com/questions/20481/incrementing-characters-in-emacs-next-to-numbers/20484
(defun increment-char-at-point ()
  "Increment number or character at point."
  (interactive)
  (condition-case nil
      (save-excursion
        (let ((chr  (1+ (char-after))))
          (unless (characterp chr) (error "Cannot increment char by one"))
          (delete-char 1)
          (insert chr)))
    (error (error "No character at point"))))

(defun increment-number-or-char-at-point ()
  "Increment number or character at point."
  (interactive)
  (let ((nump  nil))
    (save-excursion
      (skip-chars-backward "0123456789")
      (when (looking-at "[0123456789]+")
        (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))
        (setq nump  t)))
    (unless nump
      (save-excursion
        (condition-case nil
            (let ((chr  (1+ (char-after))))
              (unless (characterp chr) (error "Cannot increment char by one"))
              (delete-char 1)
              (insert chr))
          (error (error "No character at point")))))))

(defun sdh-toggle-line-move-visual (buffer-local)
  "Toggle line-move-visual."
  (interactive "P")
  ;; TODO(sdh): consider switching the default to local?
  (if buffer-local (make-variable-buffer-local 'line-move-visual))
  (setq line-move-visual (not line-move-visual)))
(setq line-move-visual nil)
(defun sdh-previous-line-visual ()
  (interactive)
  ;(if (> (point) (point-min)) (progn (backward-char) (forward-char)))
  (let ((line-move-visual t)) (backward-char) (forward-char) (previous-line)))
(defun sdh-next-line-visual ()
  (interactive)
  ;(if (> (point) (point-min)) (progn (backward-char) (forward-char)))
  (let ((line-move-visual t)) (forward-char) (backward-char) (next-line)))

;; Useful for keyboard macros
(defun sdh-search-register (reg)
  "Search forward for occurrence of the given register."
  (interactive "cRegister: ")
  (condition-case nil
      (search-forward (get-register reg))
    (error (message (format "Text not found (register %c): %s" reg (get-register reg))) (ding))))


;; Add useful ediff bindings.
(defun sdh-ediff-next-and-refine ()
  (interactive)
  (ediff-next-difference)
  (ediff-make-or-kill-fine-diffs 1))
(defun sdh-ediff-previous-and-refine ()
  (interactive)
  (ediff-previous-difference)
  (ediff-make-or-kill-fine-diffs 1))
(defun sdh-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "x" 'ediff-make-or-kill-fine-diffs)
  (define-key ediff-mode-map "j" 'sdh-ediff-next-and-refine)
  (define-key ediff-mode-map "k" 'sdh-ediff-previous-and-refine)
  )

(if (fboundp 'ediff-setup-keymap)
    (add-hook 'ediff-mode-hook 'sdh-ediff-hook))

;;;;;;;

(defun sdh-to-lower-camel ()
  (interactive)
  (downcase-word 1)
  (cond
    ((looking-at "_") (delete-char 1) (sdh-const-to-upper-camel))
    ((looking-at "[a-zA-Z]") (sdh-const-to-upper-camel))))

(defun sdh-const-to-upper-camel ()
  (interactive)
  (capitalize-word 1)
  (cond
    ((looking-at "_") (delete-char 1) (sdh-const-to-upper-camel))
    ((looking-at "[a-zA-Z]") (sdh-const-to-upper-camel))))

(defun sdh-push-point-to-column (col)
  (interactive "nColumn: ")
  (let ((count (- col (- (point) (line-beginning-position)))))
    (if (> count 0)
        (insert (make-string count ? )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Repeat the given named macro until it beeps
;; In order to pass a macro, it should probably be called
;; with M-: (sdh-repeat-macro foo), hence it is not interactive
(defun sdh-repeat-macro (macro)
  (condition-case nil (funcall macro 0) (error nil)))

(defun dirname-no-slash (path)
  "Given a PATH return the directory name (path up to but not including final slash."
  (let ((parent (file-name-directory path)))
    (if parent (directory-file-name parent)
      "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS sexp navigation
;; The main thing we want to do is navigate function arguments.
;; Simple balanced paren handling should work fine here.

(defun sdh-forward-js-sexp ()
  "Jump forward one balanced expression"
  (interactive)
  (let (stack result top in-string close next)
    (save-excursion
      (if (looking-at "[]}),;]") (forward-char))
      (while (or stack (not (looking-at "[]}),;]")))
        (setq top (and stack (car stack)))
        (setq in-string (and top (or (= top ?`) (= top ?') (= top ?\"))))
        (setq next (char-after))
        (setq close (cdr (assoc next '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})
                                       (?` . ?`) (?' . ?') (?\" . ?\")))))
        (cond ((and in-string (looking-at "\\\\"))
               ;;(message "string escape")
               (forward-char)
               (cond ((looking-at "x") (forward-char 2))
                     ((looking-at "u") (forward-char 4))
                     (t (forward-char))))
              ((and top (= top ?`) (looking-at "\\${"))
               ;;(message "template")
               (forward-char 2)
               (setq stack (cons ?} stack)))
              ((looking-at "//")
               ;;(message "eol comment")
               (end-of-line)
               (forward-char))
              ((looking-at "/\\*")
               ;;(message "block comment")
               (re-search-forward "\\*/"))
              ((and top (= top next))
               ;;(message (format "pop %c" next))
               (setq stack (cdr stack))
               ;; don't advance if closing a top-level block,
               ;; unless it's followed by semicolon
               (if (or stack (/= next ?})) (forward-char)))
              (in-string
               ;;(message "string advance")
               (forward-char))
              (close
               ;;(message (format "push %c" close))
               (setq stack (cons close stack))
               (forward-char))
              (t (forward-char))))
      (if (< (point) (point-max)) (setq result (point))))
    (cond
     (result
       (goto-char result)
       (if (looking-at "};") (forward-char))))
        ;(progn
        ;  (goto-char result)
        ;  (while (looking-at ";") (forward-char)))
))

;;; NOTE: If we want the reverse direction, we'd really need to build up a full
;;; map of start to end positions of all elements in the stack, starting at the
;;; front of the file.  Once we hit point, then jump back to the first
;;; non-whitespace char after the opening.

(provide 'sdh-misc)
