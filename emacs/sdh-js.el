;; probably only want to do this on the desktop, since
;; we have a working config elsewhere at work.

(sdh-try-require 'js2-mode)

;;; adds symbols included by google closure to js2-additional-externs
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (let ((buf (buffer-string))
                  (index 0))
              (while (string-match "\\(goog\\.require\\|goog\\.provide\\)('\\([^'.]*\\)" buf index)
                (setq index (+ 1 (match-end 0)))
                (add-to-list 'js2-additional-externs (match-string 2 buf))))))

(setq-default js2-basic-offset 2)
(setq-default js2-bounce-indent-p 't)
(setq-default js2-global-externs
              '("goog" "sdh" "window" "JSON" "process" "require" "__dirname"))
(setq-default js2-strict-trailing-comma-warning nil)

;(smart-tabs-advice js2-indent-line js2-basic-offset)

;; TODO(sdh): consider looking at the next-error-last-buffer to find the error
;; class and building up a table of suppressions?  Maybe also have a register
;; for a message that can be included?
(defun sdh-suppress-missing-properties ()
  (interactive)
  (save-excursion
    (let (end start ident comment-end comment-col)
    (re-search-forward "[^a-zA-Z0-9_$]" (line-end-position))
    (setq end (- (point) 1))
    (backward-char)
    (re-search-backward "[^a-zA-Z0-9_$.]" (line-beginning-position))
    (forward-char)
    (setq start (point))
    (setq ident (buffer-substring-no-properties start end))
    (re-search-backward "/\\*\\*")
    (re-search-forward "\\*/" start)
    (cond
     ((save-excursion (beginning-of-line) (looking-at " */\\*\\*"))
      ;; Single-line comment
      (beginning-of-line)
      (re-search-forward "\\*\\*")
      (setq comment-col (- (current-column) 2))
      (insert (format "\n%s*" (make-string comment-col ? )))
      (re-search-forward "\\*/")
      (backward-char 2)
      (insert (format "\n%s" (make-string comment-col ? ))))
     (t (setq comment-col (- (current-column) 2))))
    ;; TODO(sdh): Look for an existing @suppress? we'd need to bound by the
    ;; beginning of the current jsdoc block.
    (beginning-of-line)
    (insert (format "%s* @suppress {missingProperties} %s\n"
                    (make-string comment-col ? ) ident)))))
(global-set-key (kbd "C-c j s m") 'sdh-suppress-missing-properties)

(defun sdh-get-next-error-length ()
  (save-excursion
    ;; Basic idea: go to the error buffer and find out how long the error is?
    (let (length start end)
      (with-current-buffer next-error-last-buffer
        (save-excursion
          (re-search-forward "^ *\\^")
          (setq start (point))
          (re-search-forward "[^^]")
          (setq end (point))
          (- end start))))))
(defun sdh-get-cast-insert-length ()
  (cond
   ((and transient-mark-mode mark-active)
    (if (> (point) (mark)) (exchange-point-and-mark))
    (- (mark) (point)))
   (t (sdh-get-next-error-length))))

;; (defun sdh-insert-cast (str)
;;   (insert "/** @type {" str "} */ (")
;;   (forward-char (sdh-get-cast-insert-length))
;;   (insert ")"))

;; (defun sdh-insert-cast-to-unknown ()
;;   (interactive)
;;   (sdh-insert-cast "?"))
;; (global-set-key (kbd "C-c j s u") 'sdh-insert-cast-to-unknown)

;; (defun sdh-insert-cast-to-yank ()
;;   (interactive)
;;   (sdh-insert-cast
;;    (save-excursion
;;      (with-temp-buffer (yank) (buffer-substring-no-properties 0 (point))))))
;; (global-set-key (kbd "C-c j s c") 'sdh-insert-cast-to-yank)

(defun sdh-insert-cast-cursor ()
  (interactive)
  (message "point %d" (point))
  (let ((length (sdh-get-cast-insert-length)))
    (message "length %d point %d" length (point))
    (save-excursion
      (forward-char length)
      (insert ")"))
    (insert "/** @type {} */ (")
    (backward-char 6)))
(global-set-key (kbd "C-c j s c") 'sdh-insert-cast-cursor)

(provide 'sdh-js)
