(if (fboundp 'exec-path-from-shell-initialize)
    (progn
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "GOPATH")))

(if (file-exists-p (expand-file-name "~/local/opt/go/misc/emacs/go-mode-load.el"))
    (progn
      (setq load-path (cons (expand-file-name "~/local/opt/go/misc/emacs") load-path))
      (require 'go-mode-load)
      ;; (if (boundp 'flymake-allowed-file-name-masks)
      ;;     (add-to-list 'flymake-allowed-file-name-masks
      ;;                  '("\\.go\\'" flymake-simple-make-init)))
))

(add-hook 'go-mode-hook 'sdh-go-init)

(defun sdh-go-init ()
  "Initialization for go mode"
  (interactive)
    (set-variable 'tab-width 2)
    (unless (string-match-p "go" compile-command)   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))  ; May need to be customized for your project

    ;; guru settings
    ;(go-guru-hl-identifier-mode)                    ; highlight identifiers

    ;; Key bindings specific to go-mode
    ;(local-set-key (kbd "M-.") #'godef-jump)        ; Go to definition
    ;(local-set-key (kbd "M-*") #'pop-tag-mark)      ; Return from whence you came
    ;(local-set-key (kbd "M-p") #'compile)           ; Invoke compiler
    ;(local-set-key (kbd "M-P") #'recompile)         ; Redo most recent compile cmd
    ;(local-set-key (kbd "M-]") #'next-error)        ; Go to next error (or msg)
    ;(local-set-key (kbd "M-[") #'previous-error)    ; Go to previous error or msg

    ;; Misc go stuff
    ;(auto-complete-mode 1)                          ; Enable auto-complete mode
    (set (make-local-variable 'company-idle-delay) 1)
    (set (make-local-variable 'company-minimum-prefix-length) 1)
    (lsp-go-install-save-hooks)
    (lsp-deferred)
    (yas-minor-mode)
    (company-mode)
)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; TODO - Might be nice to get this enabled...
;; See https://github.com/dominikh/go-mode.el
;; (require 'go-guru)

(provide 'sdh-go)
