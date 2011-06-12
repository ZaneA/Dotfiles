;;
;; Theming
;;

(define (load-theme path)
  (load (conc "~/.wmii/chicken-wmii/themes/" path ".scm"))
  (add-setting font wmii-font)
  (add-setting focuscolors wmii-focuscolors)
  (add-setting normcolors wmii-normcolors))
