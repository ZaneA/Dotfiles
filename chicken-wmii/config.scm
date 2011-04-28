;;
;; Main Configuration
;;

(define wmii-term "urxvt")
(define modkey "Mod4") (add-setting grabmod modkey)

(add-setting colmode "stack")
(add-setting bar "on bottom")
(add-setting border "3")

(load-theme "live-coding")

(define directions `((up . "k") (down . "j") (left . "h") (right . "l")))


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
