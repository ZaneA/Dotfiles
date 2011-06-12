;;
;; Main Configuration
;;

(define wmii-term "urxvt")
(define modkey "Mod4") (add-setting grabmod modkey)

(add-setting colmode "stack")
(add-setting bar "on top")
(add-setting border "2")

(load-theme "dark")

(define directions `((up . "k") (down . "j") (left . "h") (right . "l")))


;; Rules

(wmii:colrules-set!
 `(("gimp"   . (13 74 13))
   ("Emacs" . (50 50))
   (".*"     . (30 58 12))))

(wmii:tagrules-set!
 `(("Quod Libet|Abraca"  . "sel+Music")
   ("MPlayer|VLC|Totem"  . ("sel+Video" "~"))
   ("Emacs"              . "sel+Emacs")
   ("Firefox|Vimperator" . "sel+Web")
   ("claws-mail"         . "sel+Web+Mail")
   ("Pidgin"             . ("sel+Web+IM" "~"))
   ("urxvt"              . "sel+Term")
   ("gimp"               . ("Graphics" "~"))
   ("Photoshop.*"        . ("Graphics" "~"))
   ("fluxus.*"           . "LiveCoding.Visual")
   ("Renoise.*"          . "LiveCoding.Audio")
   ("Mailnotify"         . ("sel" "~"))
   ("epdfview"           . ("sel" "~"))
   (".*"                 . "sel")))
