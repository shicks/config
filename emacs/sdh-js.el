;; probably only want to do this on the desktop, since
;; we have a working config elsewhere at work.

(require 'js2-mode)

;;; adds symbols included by google closure to js2-additional-externs
(add-hook 'js2-post-parse-callbacks
   (lambda ()
     (let ((buf (buffer-string))
           (index 0))
       (while (string-match "\\(goog\\.require\\|goog\\.provide\\)('\\([^'.]*\\)" buf index)
         (setq index (+ 1 (match-end 0)))
         (add-to-list 'js2-additional-externs (match-string 2 buf))))))

(setq-default js2-basic-offset 2)
(setq-default js2-bounce-indent-p 't)
(setq-default js2-global-externs '("goog" "sdh" "window" "JSON"))

;(smart-tabs-advice js2-indent-line js2-basic-offset)

(provide 'sdh-js)