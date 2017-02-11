;; Code to load flymake stuff

; Note: We might look into using the fork at
;    https://github.com/illusori/emacs-flymake-cursor
; For now, this code will work either way.

;(sdh-try-require 'flymake-cursor)

(cond
 ((fboundp 'flycheck-display-error-at-point)
  (global-set-key "\C-c\C-e" 'flycheck-display-error-at-point))
 ;; TODO(sdh): eliminate flymake...!
 ((fboundp 'flyc/show-fly-error-at-point-now)
  (global-set-key "\C-c\C-e" 'flyc/show-fly-error-at-point-now))
 ((fboundp 'flymake-cursor-show-errors-at-point-now)
  (global-set-key "\C-c\C-e" 'flymake-cursor-show-errors-at-point-now))
)
(global-set-key (kbd "C-c `") 'flymake-goto-next-error)
(global-set-key "\C-Cq" 'flymake-stop-all-syntax-checks)



(provide 'sdh-flymake)
