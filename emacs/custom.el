(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(flymake-allowed-file-name-masks (quote (("^/[uhg].+\\(/BUILD\\|\\.\\(java\\|js\\|soy\\|proto\\|html\\)\\)$" sdh-flymake-my-init flymake-simple-cleanup flymake-get-real-file-name))))
 '(google-jdb-port 5005)
 '(google-trailing-newline-modes (quote (html-mode javascript-mode closure-template-html-mode cperl-mode js2-mode protobuf-mode cc-mode c++-mode google3-build-mode python-mode emacs-lisp-mode make-mode borg-mode js-mode css-mode html-mode nxhtml-mode java-mode shell-script-mode jde-mode ess-mode Rd-mode go-mode)))
 '(google-trailing-whitespace-modes (quote (html-mode javascript-mode closure-template-html-mode cperl-mode js2-mode protobuf-mode cc-mode c++-mode google3-build-mode python-mode emacs-lisp-mode make-mode borg-mode js-mode css-mode html-mode nxhtml-mode java-mode shell-script-mode jde-mode ess-mode Rd-mode go-mode)))
 '(history-delete-duplicates t)
 '(history-length t)
 '(p4-global-key-prefix "p")
 '(savehist-additional-variables (cons (quote gud-gdb-history) savehist-additional-variables)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cperl-array-face ((((class color)) (:background "wheat" :foreground "yellow" :weight normal))))
 '(ediff-current-diff-A ((((class color)) (:background "tomato" :foreground "black"))))
 '(ediff-current-diff-B ((((class color)) (:background "pale green" :foreground "black"))))
 '(ediff-even-diff-A ((((class color)) (:background "skyblue" :foreground "black"))))
 '(ediff-even-diff-B ((((class color)) (:background "skyblue" :foreground "black"))))
 '(ediff-odd-diff-A ((((class color)) (:background "plum" :foreground "black"))))
 '(ediff-odd-diff-B ((((class color)) (:background "plum" :foreground "black"))))
 '(flymake-errline ((((class color)) (:background "color-52"))))
 '(flymake-warnline ((((class color)) (:background "color-17"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :slant italic))))
 '(font-lock-comment-face ((t (:foreground "chocolate1" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :slant italic))))
 '(region ((((class color)) (:background "skyblue2" :foreground "Black"))))
 '(trailing-whitespace ((((class color)) (:background "color-160" :foreground "Black")))))
