;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standardize terminal keyboard remappings....?

(defconst sdh-kbd-re
  (rx string-start
      (* (| (group-n 1 "S-")
            (group-n 2 "M-")
            (group-n 3 "C-")))
      (| (group-n 4 letter)
         ; TODO(sdh): match uppercase/lowercase separately?
         (group-n 5 digit)
         (group-n 6 (any "-\\`~_=+[{]}\|;:'\",<.>/?!@#$%^&*()"))
         ; TODO(sdh): F-keys, navigation keys, tab/etc
         (seq "<" (group-n 7 (| "delete" "left" "right" "up" "down")) ">")
         (group-n 8 (+ anything)))
      string-end)
  "Regex for parsing keyboard sequences")

(defconst sdh-kbd-prefix "M-[ 36~"
  "Prefix used for most extended keyboard shortcuts")

(defconst sdh-arrow-key-letters
  '(("up" . "a") ("down" . "b") ("right" . "c") ("left" . "d"))
  "Lookup table for mapping arrow keys to the right letter")

;; TODO(sdh): conditionally return just (kbd spec) if in a window?
;; TODO(sdh): consider returning a list so that we can handle
;;            the X case as well as rxvt/alacritty, etc.
(defun sdh-kbd (spec)
  "Parses an extended keysequence specification."
  (kbd
   (mapconcat
    (lambda (term)
      (if (and (not window-system) (string-match sdh-kbd-re term))
          (let* ((shift  (match-string 1 term))
                 (ctrl   (match-string 3 term))
                 (meta   (match-string 2 term))
                 (esc    (if meta " ESC " " "))
                 (letter (match-string 4 term))
                 (digit  (match-string 5 term))
                 (symbol (match-string 6 term))
                 (ds     (or digit symbol))
                 (named  (match-string 7 term))
                 (rest   (match-string 8 term))
                 ; Error cases, fall back on (kbd term)
                 (err    (or
                          ; named ; TODO(sdh): handle this later.
                          rest ; unknown final key
                          (and shift ds)))) ; use correct symbol
            (cond
             ;; 1. error cases (or just don't match them?)
             (err term)
             ;; 2. ctrl-shift-letter
             ((and ctrl shift letter)
              (format "M-[ 3 6 ~ %s C-%s" esc letter))
             ;; 3. ctrl-digit, ctrl-symbol
             ((and ctrl ds)
              (format "M-[ 3 6 ~ %s %s" esc ds))
             ;; 4. named symbols - TODO(sdh): flesh this out later
             ((and (eq named "delete") ctrl shift)
              term)
             ((assoc named sdh-arrow-key-letters)
              (let* ((num (+ 1 (if shift 1 0) (if meta 2 0) (if ctrl 4 0)))
                     (prefix (if (= num 1) "" (format "1 ; %d " num)))
                     (code (cdr (assoc named sdh-arrow-key-letters))))
                (concat "M-[ " prefix code)))
             ;; 5. all other cases are trivial, just forward as-is
             (t term)))
       term))
    (split-string spec)
    " ")))

(defun sdh-global-set-key (spec binding)
  "Parses the spec and issues multiple calls to global-set-key
to handle different terminal types."
  (interactive)
  (global-set-key (kbd spec) binding)
  (global-set-key (sdh-kbd spec) binding))

(provide 'sdh-kbd)
