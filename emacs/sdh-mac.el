;; Config for mac emacs only.

;; Swap command and option
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq default-directory "~/")

(set-default-font "Monaco")

(defun sdh-bind-compose (keys result)
  (global-set-key (kbd (concat "<f13>" keys))
                  `(lambda () (interactive) (insert ,result))))

(sdh-bind-compose "ae" "æ")
(sdh-bind-compose "AE" "Æ")
(sdh-bind-compose "< <" "«")
(sdh-bind-compose "> >" "»")
(sdh-bind-compose "< =" "≤")
(sdh-bind-compose "> =" "≥")
(sdh-bind-compose "/ =" "≠")
(sdh-bind-compose "+ -" "±")
(sdh-bind-compose "- +" "∓")
(sdh-bind-compose "! !" "¡")
(sdh-bind-compose "? ?" "¿")
(sdh-bind-compose "| c" "¢")

(global-set-key (kbd "<f13> <up>") 'previous-line)
(global-set-key (kbd "<f13> <down>") 'next-line)

(setenv "PATH" (concat (getenv "HOME") "/local/bin:" (getenv "PATH")))

(provide 'sdh-mac)
