;; Code to load flymake stuff

; Note: We might look into using the fork at
;    https://github.com/illusori/emacs-flymake-cursor
; For now, this code will work either way.

(sdh-try-require 'flymake-cursor)

(cond
 ((fboundp 'flyc/show-fly-error-at-point-now)
  (global-set-key "\C-c\C-e" 'flyc/show-fly-error-at-point-now))
 ((fboundp 'flymake-cursor-show-errors-at-point-now)
  (global-set-key "\C-c\C-e" 'flymake-cursor-show-errors-at-point-now))
)

(provide 'sdh-flymake)
