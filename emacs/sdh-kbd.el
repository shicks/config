;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standardize terminal keyboard remappings....?

;(define-key function-key-map "^[[^[[[A" (kbd "M-<f1>"))

(defconst sdh-kbd-lowercase "abcdefghijklmnopqrstuvwxyz")
(defconst sdh-kbd-uppercase "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defconst sdh-kbd-digits "0123456789")
(defconst sdh-kbd-symbols "-\\`~_=+[{]}\|;:'\",<.>/?!@#$%^&*()")
(defconst sdh-kbd-arrow-key-codes
  '(("up" . "A") ("down" . "B") ("right" . "C") ("left" . "D"))
  "Lookup table for mapping arrow keys to the right letter")

(defun sdh-kbd-map (sequence result)
  "Map sequence to result"
  ;(message "(define-key local-function-key-map (kbd \"%s\") (kbd \"%s\"))" sequence result)
  (define-key local-function-key-map (kbd sequence) (kbd result)))

(defun sdh-kbd-map-ctrl-shift-letter (letter)
  "Map C-S-letter and C-S-M-letter."
  (let ((letter (string letter)))
    (sdh-kbd-map (format "M-[ 3 6 ~ C-%s" letter) (format "C-S-%s" letter))
    (sdh-kbd-map (format "M-[ 3 6 ~ ESC C-%s" letter) (format "M-C-S-%s" letter))))

(defun sdh-kbd-map-ctrl-digit-or-symbol (digit)
  "Map C-digit and C-symbol, with and without meta."
  (let ((digit (string digit)))
    (sdh-kbd-map (format "M-[ 3 6 ~ %s" digit) (format "C-%s" digit))
    (sdh-kbd-map (format "M-[ 3 6 ~ ESC %s" digit) (format "M-C-%s" digit))))

; NOTE: This was a maddening issue to fix.  It turns out that capitalization
; sometimes matters in bindings, but not always.  I was binding M-[1;7d to C-M-<left>,
; but my terminal was sending a capital D instead.  But when Emacs reported the
; unbound key, it told me "M-[1;7d" was unbound, which was a lie.  To figure out
; the specific keys, I used "M-x view-lossage" (C-h l).  See also open-dribble-file.
(defun sdh-kbd-map-modified-arrow-keys (cell)
  "Map various combinations of control keys with arrows."
  (let ((key (car cell))
        (code (cdr cell)))
    (sdh-kbd-map (format "M-[ 1 ; 2 %s" code) (format "S-<%s>" key))
    (sdh-kbd-map (format "M-[ 1 ; 3 %s" code) (format "M-<%s>" key))
    (sdh-kbd-map (format "M-[ 1 ; 4 %s" code) (format "M-S-<%s>" key))
    (sdh-kbd-map (format "M-[ 1 ; 5 %s" code) (format "C-<%s>" key))
    (sdh-kbd-map (format "M-[ 1 ; 6 %s" code) (format "C-S-<%s>" key))
    (sdh-kbd-map (format "M-[ 1 ; 7 %s" code) (format "C-M-<%s>" key))
    (sdh-kbd-map (format "M-[ 1 ; 8 %s" code) (format "C-M-S-<%s>" key))))

(defun sdh-kbd-init ()
  (interactive)
  (mapcar 'sdh-kbd-map-ctrl-shift-letter sdh-kbd-uppercase)
  (mapcar 'sdh-kbd-map-ctrl-digit-or-symbol sdh-kbd-digits)
  (mapcar 'sdh-kbd-map-ctrl-digit-or-symbol sdh-kbd-symbols)
  (mapcar 'sdh-kbd-map-modified-arrow-keys sdh-kbd-arrow-key-codes))


;; (defconst sdh-kbd-re
;;   (rx string-start
;;       (* (| (group-n 1 "S-")
;;             (group-n 2 "M-")
;;             (group-n 3 "C-")))
;;       (| (group-n 4 letter)
;;          ; TODO(sdh): match uppercase/lowercase separately?
;;          (group-n 5 digit)
;;          (group-n 6 (any "-\\`~_=+[{]}\|;:'\",<.>/?!@#$%^&*()"))
;;          ; TODO(sdh): F-keys, navigation keys, tab/etc
;;          (seq "<" (group-n 7 (| "delete" "left" "right" "up" "down")) ">")
;;          (group-n 8 (+ anything)))
;;       string-end)
;;   "Regex for parsing keyboard sequences")

;; (defconst sdh-kbd-prefix "M-[ 36~"
;;   "Prefix used for most extended keyboard shortcuts")

;; ;; TODO(sdh): conditionally return just (kbd spec) if in a window?
;; ;; TODO(sdh): consider returning a list so that we can handle
;; ;;            the X case as well as rxvt/alacritty, etc.
;; (defun sdh-kbd (spec)
;;   "Parses an extended keysequence specification."
;;   (kbd
;;    (mapconcat
;;     (lambda (term)
;;       (if (and (not window-system) (string-match sdh-kbd-re term))
;;           (let* ((shift  (match-string 1 term))
;;                  (ctrl   (match-string 3 term))
;;                  (meta   (match-string 2 term))
;;                  (esc    (if meta " ESC " " "))
;;                  (letter (match-string 4 term))
;;                  (digit  (match-string 5 term))
;;                  (symbol (match-string 6 term))
;;                  (ds     (or digit symbol))
;;                  (named  (match-string 7 term))
;;                  (rest   (match-string 8 term))
;;                  ; Error cases, fall back on (kbd term)
;;                  (err    (or
;;                           ; named ; TODO(sdh): handle this later.
;;                           rest ; unknown final key
;;                           (and shift ds)))) ; use correct symbol
;;             (cond
;;              ;; 1. error cases (or just don't match them?)
;;              (err term)
;;              ;; 2. ctrl-shift-letter
;;              ((and ctrl shift letter)
;;               (format "M-[ 3 6 ~ %s C-%s" esc letter))
;;              ;; 3. ctrl-digit, ctrl-symbol
;;              ((and ctrl ds)
;;               (format "M-[ 3 6 ~ %s %s" esc ds))
;;              ;; 4. named symbols - TODO(sdh): flesh this out later
;;              ((and (eq named "delete") ctrl shift)
;;               term)
;;              ((assoc named sdh-arrow-key-letters)
;;               (let* ((num (+ 1 (if shift 1 0) (if meta 2 0) (if ctrl 4 0)))
;;                      (prefix (if (= num 1) "" (format "1 ; %d " num)))
;;                      (code (cdr (assoc named sdh-arrow-key-letters))))
;;                 (concat "M-[ " prefix code)))
;;              ;; 5. all other cases are trivial, just forward as-is
;;              (t term)))
;;        term))
;;     (split-string spec)
;;     " ")))

;; (defun sdh-global-set-key (spec binding)
;;   "Parses the spec and issues multiple calls to global-set-key
;; to handle different terminal types."
;;   (interactive)
;;   (global-set-key (kbd spec) binding)
;;   (global-set-key (sdh-kbd spec) binding))

(provide 'sdh-kbd)
