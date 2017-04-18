(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(cperl-indent-parens-as-block t)
 '(flymake-allowed-file-name-masks
   (quote
    (("^/[uhg].+\\(/BUILD\\|\\.\\(java\\|js\\|soy\\|proto\\|html\\)\\)$" sdh-flymake-my-init flymake-simple-cleanup flymake-get-real-file-name))))
 '(google-jdb-port 5005)
 '(google-trailing-newline-modes
   (quote
    (html-mode javascript-mode closure-template-html-mode cperl-mode js2-mode protobuf-mode cc-mode c++-mode google3-build-mode python-mode emacs-lisp-mode make-mode borg-mode js-mode css-mode html-mode nxhtml-mode java-mode shell-script-mode jde-mode ess-mode Rd-mode go-mode)))
 '(google-trailing-whitespace-modes
   (quote
    (html-mode javascript-mode closure-template-html-mode cperl-mode js2-mode protobuf-mode cc-mode c++-mode google3-build-mode python-mode emacs-lisp-mode make-mode borg-mode js-mode css-mode html-mode nxhtml-mode java-mode shell-script-mode jde-mode ess-mode Rd-mode go-mode)))
 '(history-delete-duplicates t)
 '(history-length t)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-mode-assume-strict t)
 '(js2-skip-preprocessor-directives t)
 '(menu-bar-mode nil)
 '(p4-global-key-prefix "p")
 '(perl6-indent-offset 2)
 '(savehist-additional-variables
   (cons
    (quote gud-gdb-history)
    savehist-additional-variables)))

;;; NOTE: '(cperl-array-face ((t (:background "wheat" :foreground "yellow" :weight normal))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:background "wheat" :foreground "Brown" :weight normal))))
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
 '(print-tag-face ((t (:foreground "brightyellow"))) t)
 '(region ((((class color)) (:background "skyblue2" :foreground "Black"))))
 '(trailing-whitespace ((((class color)) (:background "color-160" :foreground "Black"))))
 '(whitespace-line ((t (:background "color-52")))))
