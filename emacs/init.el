;;; Main emacs configuration
;;
;; This is a short file that loads other files, and is shared
;; across all machines.

;; Set up load path
(add-to-list 'load-path "~/config/emacs")

;; My own settings
(require 'sdh-misc)
(require 'sdh-keys)
(require 'sdh-sh)
(require 'sdh-tmux)
(require 'sdh-colors)

;; Language-specific settings
(require 'sdh-perl)
(require 'sdh-git)
(require 'closure-template-html-mode)

(if (string= system-name "daneel")
    ;(load-file "/usr/share/emacs/site-lisp/ledger/ledger.el")
    (require 'sdh-ledger)
    (require 'sdh-js)
)

(if (string= system-name "sdh-glaptop")
    ; TODO(sdh): figure out how to do this mor consistently.
    (set-default-font "DejaVu Sans Mono-8")
)

(if (boundp 'mac-command-modifier)
    (require 'sdh-mac)
)

;(require 'sdh-flymake)
(require 'sdh-go)
(require 'fill-column-indicator)

(if (file-exists-p (expand-file-name "~/local/opt/emacs"))
    (setq load-path (cons (expand-file-name "~/local/opt/emacs") load-path)))

;; Custom configuration settings go in their own file.
(setq custom-file "~/config/emacs/custom.el")
(load custom-file)

;; kmacro-decision beefs up (C-x q), allowing custom conditional branches
;;;;;;  - consider fixing this up a bit more, or else binding it to C-x M-q
;;;;;;    so that we don't lose the normal kbd-macro-query
;(add-to-list 'load-path "~/config/emacs/kmacro-decision")
;(require 'kmacro-decision)
