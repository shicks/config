;; TODO(sdh): make easy-to-toggle switches so we can go between terminal and window mode faces...

; Window modE:
;(custom-set-faces
; '(region ((((class color)) (:background "lightgoldenrod2"))))     ;(:background "color-52"))))
; '(flymake-errline ((((class color)) (:background "LightPink"))))
; '(flymake-warnline ((((class color)) (:background "LightBlue2")))))


; Terminal mode: also consider (:inverse-video t :bold t) or maybe just specify foreground too?
(custom-set-faces
; '(region ((((class color)) (:inverse-video t))))
; '(region ((((class color)) (:background "lightgoldenrod2" :foreground "Black"))))
 '(region ((((class color)) (:background "skyblue2" :foreground "Black"))))
 '(trailing-whitespace ((((class color)) (:background "color-160" :foreground "Black"))))
 '(flymake-errline ((((class color)) (:background "color-52"))))
 '(flymake-warnline ((((class color)) (:background "color-17"))))

; '(ediff-current-diff-A ((((class color)) (:background "brightwhite" :foreground "firebrick"))))
; '(ediff-current-diff-B ((((class color)) (:background "brightwhite" :foreground "dark green"))))
 ;'(ediff-even-diff-A ((((class color)) (:background "plum" :foreground "black"))))
 ;'(ediff-even-diff-B ((((class color)) (:background "aquamarine" :foreground "black"))))
; '(ediff-odd-diff-A ((((class color)) (:background "tomato" :foreground "black"))))
; '(ediff-odd-diff-B ((((class color)) (:background "pale green" :foreground "black"))))
 '(ediff-current-diff-A ((((class color)) (:background "tomato" :foreground "black"))))
 '(ediff-current-diff-B ((((class color)) (:background "pale green" :foreground "black"))))
 '(ediff-even-diff-A ((((class color)) (:background "skyblue" :foreground "black"))))
 '(ediff-even-diff-B ((((class color)) (:background "skyblue" :foreground "black"))))
 '(ediff-odd-diff-A ((((class color)) (:background "plum" :foreground "black"))))
 '(ediff-odd-diff-B ((((class color)) (:background "plum" :foreground "black"))))
)

;; These are temporary, until we figure out WTF is wrong with the colors!!!
;(custom-set-faces
; '(mode-line ((((class color)) (:background "white" :foreground "black"))))
; '(mode-line-inactive ((((class color)) (:foreground "brightblack" :background "black"))))
; '(flymake-errline ((((class color)) (:background "red" :foreground "black"))))
; '(flymake-warnline ((((class color)) (:background "blue" :foreground "white"))))
;)

; '(flymake-errline ((((class color)) (:background "brightred" :foreground "brightwhite"))))
; '(flymake-warnline ((((class color)) (:background "brightblue" :foreground "brightwhite"))))
;)
;(set-face-background 'default "Black") ;; this is a fix for flymake...?
;(set-face-foreground 'default "brightwhite") ;; this is a fix for flymake...?

(xterm-mouse-mode t)

;(set-cursor-color "")
;(set-background-color "Black")
;(set-foreground-color "white")

;(set-cursor-color "Black")
;(set-face-background 'default "brightwhite") ;; this is a fix for flymake...?
;(set-background-color "brightwhite")
;(set-foreground-color "Black")
;; consider brightwhite


(provide 'sdh-colors)
