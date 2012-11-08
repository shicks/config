;;; Main emacs configuration
;;
;; This is a short file that loads other files, and is shared
;; across all machines.

;; Set up load path
(add-to-list 'load-path "~/config/emacs")

;; First bring in standard Google settings
(require 'google)

;; My own settings
(require 'sdh-misc)
(require 'sdh-keys)
(require 'sdh-tmux)
(require 'sdh-colors)

;; Language-specific settings
(require 'sdh-perl)
(require 'sdh-git)
  ; TODO(sdh): ledger

;; Custom configuration settings go in their own file.
(setq custom-file "~/config/emacs/custom.el")
(load custom-file)
