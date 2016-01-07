;; Shell mode

(setq sh-styles-alist
      '(("sdh-sh-style"
         (sh-basic-offset . 2)
         (sh-first-lines-indent . 0)
         (sh-indent-after-case . +)
         (sh-indent-after-do . +)
         (sh-indent-after-done . 0)
         (sh-indent-after-else . +)
         (sh-indent-after-if . +)
         (sh-indent-after-loop-construct . +)
         (sh-indent-after-open . +)
         (sh-indent-comment . t)
         (sh-indent-for-case-alt . ++)
         (sh-indent-for-case-label . +)
         (sh-indent-for-continuation . +)
         (sh-indent-for-do . 0)
         (sh-indent-for-done . 0)
         (sh-indent-for-else . 0)
         (sh-indent-for-fi . 0)
         (sh-indent-for-then . -2))))

(defun sdh-sh-init ()
  "Initialization for sh-mode"
  (interactive)
  (sh-load-style "sdh-sh-style"))

(add-hook 'sh-set-shell-hook 'sdh-sh-init)

(provide 'sdh-sh)
