; Defines a modifier insertion mode that can insert various modifiers easily

(define-minor-mode java-modifier-mode
  "Toggles java modifier mode."
  nil ;; initial value
  " MOD" ;; status line indicator
  ;; bindings
  '(("o" . (insert "@Override "))
    ("p" . (insert "public "))
    ("s" . (insert "static "))
    ("\C-="))
