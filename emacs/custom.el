(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-basic-offset 2)
 '(column-number-mode t)
 '(compilation-error-regexp-alist
   (quote
    (google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback google3-build-log-parser-info google3-build-log-parser-warning google3-build-log-parser-error google3-build-log-parser-python-traceback google-blaze-error google-blaze-warning google-log-error google-log-warning google-log-info google-log-fatal-message google-forge-python gunit-stack-trace absoft ada aix ant bash borland python-tracebacks-and-caml cmake cmake-info comma cucumber msft edg-1 edg-2 epc ftnchek iar ibm irix java jikes-file maven jikes-line clang-include clang-include gcc-include ruby-Test::Unit gnu lcc makepp mips-1 mips-2 msft omake oracle perl php rxp sparc-pascal-file sparc-pascal-line sparc-pascal-example sun sun-ada watcom 4bsd gcov-file gcov-header gcov-nomark gcov-called-line gcov-never-called perl--Pod::Checker perl--Test perl--Test2 perl--Test::Harness weblint guile-file guile-line)))
 '(cperl-indent-parens-as-block t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(flymake-allowed-file-name-masks
   (quote
    (("^/[uhg].+\\(/BUILD\\|\\.\\(java\\|js\\|soy\\|proto\\|html\\)\\)$" sdh-flymake-my-init flymake-simple-cleanup flymake-get-real-file-name))))
 '(flymake-proc-allowed-file-name-masks
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
 '(js2-mode-assume-strict t)
 '(js2-skip-preprocessor-directives t)
 '(menu-bar-mode nil)
 '(p4-global-key-prefix "p")
 '(package-selected-packages
   (quote
    (company-lsp dash doremi-cmd exec-path-from-shell flycheck haskell-mode hl-line+ js2-mode ledger-mode lsp-mode lsp-typescript lsp-ui markdown-mode multiple-cursors phi-search phi-search-mc protobuf-mode rainbow-mode rust-mode tide typescript-mode visual-regexp window-jump yasnippet)))
 '(perl6-indent-offset 2)
 '(python-indent-offset 2)
 '(savehist-additional-variables
   (cons
    (quote gud-gdb-history)
    savehist-additional-variables))
 '(typescript-indent-level 2))

;;; NOTE: '(cperl-array-face ((t (:background "wheat" :foreground "yellow" :weight normal))))
;;;       '(cperl-array-face ((((class color)) (:background "wheat" :foreground "yellow" :weight normal))))
;;;       '(cperl-array-face ((t (:background "wheat" :foreground "Brown" :weight normal))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:background "wheat" :foreground "chocolate4" :weight normal))))
 '(ediff-current-diff-A ((((class color)) (:background "tomato" :foreground "black"))))
 '(ediff-current-diff-B ((((class color)) (:background "pale green" :foreground "black"))))
 '(ediff-even-diff-A ((((class color)) (:background "skyblue" :foreground "black"))))
 '(ediff-even-diff-B ((((class color)) (:background "skyblue" :foreground "black"))))
 '(ediff-odd-diff-A ((((class color)) (:background "plum" :foreground "black"))))
 '(ediff-odd-diff-B ((((class color)) (:background "plum" :foreground "black"))))
 '(flymake-errline ((((class color)) (:background "color-52"))) t)
 '(flymake-warnline ((((class color)) (:background "color-17"))) t)
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :slant italic))))
 '(font-lock-comment-face ((t (:foreground "chocolate1" :slant italic))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :slant italic))))
 '(hl-line ((t (:inherit nil :background "orange4"))))
 '(isearch-fail ((t (:background "indianred3"))))
 '(print-tag-face ((t (:foreground "brightyellow"))))
 '(region ((((class color)) (:background "skyblue2" :foreground "Black"))))
 '(smerge-base ((t (:background "goldenrod4"))))
 '(smerge-lower ((t (:background "darkseagreen4"))))
 '(smerge-refined-added ((t (:background "seagreen4"))))
 '(smerge-refined-removed ((t (:background "firebrick3"))))
 '(smerge-upper ((t (:background "firebrick4"))))
 '(trailing-whitespace ((((class color)) (:background "color-160" :foreground "Black"))))
 '(vc-edited-state ((t (:foreground "black" :box (:line-width -1 :style released-button)))))
 '(vc-up-to-date-state ((t (:foreground "black" :box (:line-width -1 :style released-button)))))
 '(whitespace-line ((t (:background "color-52")))))
