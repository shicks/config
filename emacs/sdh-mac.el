;; Config for mac emacs only.

;; Swap command and option
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq default-directory "~/")

;(set-default-font "Monaco")
;(set-default-font "monofur-13")
;(set-default-font "Monofur Nerd Font-13")

(defun sdh-inserter (result)
  `(lambda () (interactive) (insert ,result)))

(global-set-key (kbd "<f13> a e") (sdh-inserter "æ"))
(global-set-key (kbd "<f13> A E") (sdh-inserter "Æ"))
(global-set-key (kbd "<f13> < <") (sdh-inserter "«"))
(global-set-key (kbd "<f13> > >") (sdh-inserter "»"))
(global-set-key (kbd "<f13> < =") (sdh-inserter "≤"))
(global-set-key (kbd "<f13> > =") (sdh-inserter "≥"))
(global-set-key (kbd "<f13> / =") (sdh-inserter "≠"))
(global-set-key (kbd "<f13> + -") (sdh-inserter "±"))
(global-set-key (kbd "<f13> - +") (sdh-inserter "∓"))
(global-set-key (kbd "<f13> ! !") (sdh-inserter "¡"))
(global-set-key (kbd "<f13> ? ?") (sdh-inserter "¿"))
(global-set-key (kbd "<f13> | c") (sdh-inserter "¢"))

(global-set-key (kbd "<f13> <up>") 'previous-line)
(global-set-key (kbd "<f13> <down>") 'next-line)

(setenv "PATH" (concat (getenv "HOME") "/local/bin:" (getenv "PATH")))

(when (fboundp 'exec-path-from-shell-initialize)
    (exec-path-from-shell-initialize))
(provide 'sdh-mac)
