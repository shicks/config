;; probably only want to do this on the desktop, since
;; we have a working config elsewhere at work.

(sdh-try-require 'js2-mode)

;;; adds symbols included by google closure to js2-additional-externs
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (let ((buf (buffer-string))
                  (index 0))
              (while (string-match "\\(goog\\.require\\|goog\\.provide\\)('\\([^'.]*\\)" buf index)
                (setq index (+ 1 (match-end 0)))
                (add-to-list 'js2-additional-externs (match-string 2 buf))))))

(defun sdh-js2-mode-init ()
  "Applies custom initialization"
  (interactive)
  (local-set-key (kbd "<tab>") 'js2-indent-bounce))
       
(add-hook 'js2-mode-hook

(setq-default js2-basic-offset 2)
(setq-default js2-bounce-indent-p 't)
(setq-default js2-global-externs
              '("goog" "sdh" "window" "JSON" "process" "require" "__dirname"))
(setq-default js2-strict-trailing-comma-warning nil)

;(smart-tabs-advice js2-indent-line js2-basic-offset)

(provide 'sdh-js)
