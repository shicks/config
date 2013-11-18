;; TODO(sdh): figure out what this is supposed to be.
;; (i.e. set it to auto-load)
;(setq load-path (cons (expand-file-name "~/.emacs-lisp/") load-path))
;(setq load-path (cons (expand-file-name "/usr/share/emacs/site-lisp/ledger") load-path))

;(load "ledger-mode") ;; Note: added some bug fixes to my own copy
;(provide 'ledger') ;; does this work?

(if (file-exists-p (expand-file-name "~/local/opt/go/misc/emacs/go-mode-load.el"))
    (progn
      (setq load-path (cons (expand-file-name "~/local/opt/ledger/emacs") load-path))
      (require 'ledger)
      (custom-set-faces
       '(ledger-font-xact-highlight-face ((((class color)) (:background "brightblack"))))
       ))

(provide 'sdh-ledger)
