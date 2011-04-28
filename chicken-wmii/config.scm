;;
;; Main Configuration
;;

(define modkey "Mod4")

(define wmii-colmode "stack")
(define wmii-barposition "on bottom")
(define wmii-floatborder "3")
(define wmii-term "urxvt")

(define directions
  `((up    . "k")
    (down  . "j")
    (left  . "h")
    (right . "l")))


;; Rules

(wmii:colrules-set!
 `(("gimp"   . (17 83 41))
   (".*"     . (38 62))))

(wmii:tagrules-set!
 `(("Quod Libet|Abraca"  . "sel+Music")
   ("MPlayer|VLC|Totem"  . ("sel+Video" "~"))
   ("Emacs"              . "sel+Emacs")
   ("Firefox|Vimperator" . "sel+Web")
   ("claws-mail"         . "sel+Web+Mail")
   ("urxvt"              . "sel+Term")
   ("gimp"               . ("Graphics" "~"))
   ("Photoshop.*"        . ("Graphics" "~"))
   ("fluxus.*"           . "LiveCoding.Visual")
   ("Renoise.*"          . "LiveCoding.Audio")
   (".*"                 . "sel")))
