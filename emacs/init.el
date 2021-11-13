;;; Main emacs configuration
;;
;; This is a short file that loads other files, and is shared
;; across all machines.

;; TODO(sdh): look into https://github.com/jwiegley/use-package

;; Set up load path
(add-to-list 'load-path "~/config/emacs")

;; My own settings
(require 'sdh-misc)
(require 'sdh-keys)
(require 'sdh-sh)
(require 'sdh-tmux)
(require 'sdh-colors)
(require 'sdh-ts)
;(require 'sdh-color-theme)
;(sdh-color-theme)

;(if (fboundp 'dirname-no-slash) (require 'sdh-repo))
;(require 'sdh-color-theme)

; non-work computers don't need this as badly, and it's broken.  skip.
(cond
 ((string= system-name "DESKTOP-ONQGME9")) ; NOTE: this is the WSL hostname)
 ((string= system-type "darwin")) ; do nothing on mac
 (t (require 'sdh-repo)))

;; Helpful for getting mac path correct - must install the eponymous package.
(if (fboundp 'exec-path-from-shell-initialize) (exec-path-from-shell-initialize))

;; Language-specific settings
(require 'sdh-perl)
(require 'sdh-git)
(require 'sdh-hg)

(require 'sdh-js)

(load "term/rxvt")  ;; TODO - gnome instead of rxvt?
(if (boundp 'term-file-aliases)
    (progn
      (add-to-list 'term-file-aliases '("tmux" . "rxvt"))
      (add-to-list 'term-file-aliases '("alacritty" . "rxvt"))))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(foreground-color . "white"))
;;;;;(add-to-list 'default-frame-alist '(foreground-color . "gray"))
(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 120))

; Note: the nerd font (which has powerline and bold) uses a different name for
; each font in the family, which breaks italics.  So just use the original.
(if (string= system-type "darwin")
    (add-to-list 'default-frame-alist '(font . "Monofur Nerd Font"))
  (add-to-list 'default-frame-alist '(font . "monofur")))

;;;;;;;(set-face-attribute 'default nil :family "Monofur Nerd Font")
;(set-face-attribute 'font-lock-comment-face nil :family "Monofuritalic Nerd Font")

(if (string= system-name "daneel")
    ;(load-file "/usr/share/emacs/site-lisp/ledger/ledger.el")
    (require 'sdh-ledger))

(if (string= system-type "darwin")
    ; TODO(sdh): figure out how to do this more consistently.
    (progn (require 'sdh-mac))
  (progn (require 'sdh-linux)))

;;This was useful for ubuntu laptop...
;(set-default-font "DejaVu Sans Mono-8")

(if (not (string= system-type "darwin"))
    ;; NOTE: These don't currently work on mac.
    (progn (require 'closure-template-html-mode)
           (require 'sdh-flymake)
           ))

(require 'sdh-go)
(require 'fill-column-indicator)

(if (file-exists-p (expand-file-name "~/local/opt/emacs"))
    (setq load-path (cons (expand-file-name "~/local/opt/emacs") load-path)))

;; Custom configuration settings go in their own file.
(setq custom-file "~/config/emacs/custom.el")
(load custom-file)

(electric-indent-mode 0)
