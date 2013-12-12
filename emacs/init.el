;;; Main emacs configuration
;;
;; This is a short file that loads other files, and is shared
;; across all machines.

;; Set up load path
(add-to-list 'load-path "~/config/emacs")

;; My own settings
(require 'sdh-misc)
(require 'sdh-keys)
(require 'sdh-tmux)
(require 'sdh-colors)

;; Language-specific settings
(require 'sdh-perl)
(require 'sdh-git)
(require 'closure-template-html-mode)

(if (string= system-name "daneel")
    ;(load-file "/usr/share/emacs/site-lisp/ledger/ledger.el")
    (require 'sdh-ledger)
)

(require 'sdh-flymake)
(require 'sdh-go)

(if (file-exists-p (expand-file-name "~/local/opt/emacs"))
    (setq load-path (cons (expand-file-name "~/local/opt/emacs") load-path)))

;; Custom configuration settings go in their own file.
(setq custom-file "~/config/emacs/custom.el")
(load custom-file)
