;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Perl mode

;; Use cperl-mode instead of the default perl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;;; (defalias 'perl-mode 'cperl-mode) ;; Brevity is the soul of wit
;; Change some defaults
(add-hook 'cperl-mode-hook 'n-cperl-mode-hook t)
(defun n-cperl-mode-hook ()
  (interactive)
  (setq cperl-indent-level 2)
;  (setq cperl-invalid-face nil) ;; Turn off the trailing whitespace indicator
  (setq cperl-hairy nil)
;  (setq cperl-hairy t) ;; Turns on most of the CPerlMode options
;  (setq cperl-continued-statement-offset 0)
;  (setq cperl-extra-newline-before-brace t)
  (set-face-background 'cperl-array-face "wheat")
  (set-face-background 'cperl-hash-face "wheat")
  (cperl-define-key "}" 'cperl-electric-terminator)
  (cperl-init-faces)
  ;; don't align parens!
  (custom-set-variables '(cperl-indent-parens-as-block t))
)
;; (cperl-init-faces) ;; Do we need this? - it seems to do something...

(defface cperl-my-trailing-spaces-face
  '((((class color))
     (
      :background "color-236" ; grey
      ; :underline "grey"
      )
     ))
  "My face for trailing spaces in cperl mode"
  :group 'cperl-mode)

(set-default 'cperl-invalid-face 'cperl-my-trailing-spaces-face)

(provide 'sdh-perl)
