(if (file-exists-p (expand-file-name "~/local/opt/go/misc/emacs/go-mode-load.el"))
    (progn
      (setq load-path (cons (expand-file-name "~/local/opt/go/misc/emacs") load-path))
      (require 'go-mode-load)
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.go\\'" flymake-simple-make-init))
))

(add-hook 'go-mode-hook 'sdh-go-init)

(defun sdh-go-init ()
  "Initialization for go mode"
  (interactive)
    (set-variable 'tab-width 2))

(provide 'sdh-go)
