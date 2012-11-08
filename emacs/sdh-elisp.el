;;;;;;;
;; For navigating elisp, M-. looks for a definition in
;; the same file (maybe there's a better way?)

(defun find-defun-from-point ()
  "Finds the definition of the symbol at point"
  (interactive)
  (let ((defun-pos)
        (function-name (current-word)))
    (save-excursion
      (beginning-of-buffer)
      (setq defun-pos (search-forward-regexp
          (concat "([ \t]*def[a-z]+[ \t]+" function-name "\\b"))))
    (if defun-pos
        (goto-char defun-pos))))

(add-hook 'emacs-lisp-mode-hook
  (lambda ()
    (define-key emacs-lisp-mode-map
      "\M-." 'find-defun-from-point)))
