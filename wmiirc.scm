#!/usr/bin/csi -s

;;; Zanes wmii config, in Chicken Scheme
;;; Modified from the wmii egg example (I guess that goes without saying)
;;;
;;; TODO
;;; Stabilize it a bit. It's not bad but occasionally I get exceptions in places

(use (prefix wmiirc wmii:) srfi-18 posix regex regex-literals srfi-13 numspell)

(wmii:connect)


;;; Main config

(define modkey "Mod4")

(define directions
  `((up    . "k")
    (down  . "j")
    (left  . "h")
    (right . "l")))

;(define wmii-normcolors  '(#xf6f3e8 #x242424 #x303030)) ; lime
;(define wmii-focuscolors '(#x000000 #xcae682 #x303030)) ; lime

;(define wmii-normcolors  '(#xf3bbc3 #x242424 #x303030)) ; red
;(define wmii-focuscolors '(#xf3bbc3 #xc24c5b #x303030)) ; red

(define wmii-normcolors  '(#xccd9c8 #x242424 #x303030)) ; dA green
(define wmii-focuscolors '(#xccd9c8 #x40534a #x303030)) ; dA green

;(define wmii-font "xft:Terminus-7")
;(define wmii-font "xft:Helvetica Neue-8:style=Medium")
(define wmii-font "xft:Geneva-8")
;
(define wmii-colmode "default")
(define wmii-barposition "on bottom")
(define wmii-floatborder "3")
(define wmii-term "urxvt")

(define wmii-evalcolors '(#xaa8888 #x242424 #x242424))

;;; Tools

; small functions to help read files
(define file/read-lines (cut with-input-from-file <> read-lines))
(define (file/read-line file) (car (file/read-lines file)))

; I use this to format my Mail status
(define (number->english* number str)
  (string-titlecase
    (cond ((zero? number)
           (format "no ~as" str))
          ((eq? 1 number)
           (format "~a ~a" (number->english number) str))
          (else
            (format "~a ~as" (number->english number) str)))))

; Start a process in the background
(define (fork-run . args)
  (process-wait (process-fork (lambda () (apply process-run args)))))

; Start a process and return its output
(define (shell args)
  (with-input-from-pipe args read-lines))

; Nth
(define-syntax nth
  (syntax-rules ()
    ((nth i list)
     (list-ref list i))))


(define *status-bar-interval* 2)

(define status-bars
  `((a-email (string-append
               "Mail: "
               (number->english*
                 (string->number (string-join (shell "claws-mail --status | awk '{print $2}'")))
                 "message"))
             #f
             (#xffffff #x242424 #x242424))
    (b-conky (car (shell "conky -c ~/.wmii/conky"))
             #f
             (#xcc8888 #x242424 #x242424))
    (c-music (file/read-line "~/.musicd/current")
             (fork-run "nautilus")
             (#xccd9c8 #x40534a #x303030))
    (d-loadavg (car (shell "awk '{print $1\" \"$2\" \"$3}' /proc/loadavg"))
               (fork-run (conc wmii-term " -e htop"))
               (#x8888cc #x242424 #x242424))
    (e-date (time->string (seconds->local-time) "%A %e %B %l.%M%P")
            #f
            (#xcccc88 #x242424 #x242424))))


;;; Rule config

(wmii:colrules-set!
 `(("gimp"   . (17 83 41))
   (".*"     . (38 62))))

(wmii:tagrules-set!
 `(("Quod Libet|Abraca"  . "1+Music")
   ("MPlayer|VLC|Totem"  . ("1+Video" "~"))
   ("Emacs"              . "1+Emacs")
   ("Firefox|Vimperator" . "1+Web")
   ("urxvt"              . "1+Term")
   ("gimp"               . "Graphics")
   (".*"                 . "sel")))

(wmii:global-settings-set!
 `((font        . ,wmii-font)
   (focuscolors . ,wmii-focuscolors)
   (normcolors  . ,wmii-normcolors)
   (grabmod     . ,modkey)
   (colmode     . ,wmii-colmode)
   (bar         . ,wmii-barposition)
   (border      . ,wmii-floatborder)))


;;; Function definitions

(define (wimenu options . rest)
  (receive (in out pid)
           (process "wimenu")
           (display (string-join options "\n") out)
           (close-output-port out)
           (let ((chosen (read-line in)))
             (close-input-port in)
             (and (string? chosen) chosen))))

(define status
  (let ((status-thread #f))
    (lambda ()
      (and status-thread (thread-terminate! status-thread))
      (set! status-thread
	    (thread-start!
	     (make-thread
	      (lambda ()
		(let loop ()
                  (for-each
                   (lambda (bar)
                     (wmii:write-tab "rbar" (nth 0 bar) (string-trim-both (eval (nth 1 bar))) (nth 3 bar)))
                   (append status-bars `((0 "" #f ,wmii-normcolors))))
		  (thread-sleep! *status-bar-interval*)
		  (loop)))))))))

(define (proglist path)
  (sort!
   (delete-duplicates!
    (flatten
     (map (lambda (dir)
            (if ((conjoin directory? file-execute-access? file-read-access?) dir)
                (map pathname-strip-directory
                     (find-files dir (conjoin (complement directory?) file-execute-access?) cons '() 0))
                '()))
          path))
    string=?)
   string<?))

(define programs '())
(define (update-programs)
  (thread-start!
   (make-thread
    (lambda () (set! programs (proglist (string-split (getenv "PATH") ":")))))))
(update-programs)


;;; Event handling

(wmii:event-handlers-set!
 `(
   ;; Main Events
   (create-tag
    . ,(lambda (event tag) (wmii:write-tab "lbar" tag tag wmii-normcolors)))
   (destroy-tag
    . ,(lambda (event tag) (wmii:destroy-tab "lbar" tag)))
   (focus-tag
    . ,(lambda (event tag)
         (if (member tag (wmii:tabs "lbar"))
             (wmii:write-tab "lbar" tag tag wmii-focuscolors))))
   (unfocus-tag
    . ,(lambda (event tag)
         (if (member tag (wmii:tabs "lbar"))
             (wmii:write-tab "lbar" tag tag wmii-normcolors))))
   (urgent-tag
    . ,(lambda (event tag client?) (wmii:write-tab "lbar" tag (string-append "*" tag))))
   (not-urgent-tag
    . ,(lambda (event tag client?) (wmii:write-tab "lbar" tag tag)))
   (left-bar-click
    . ,(lambda (event button tab) (wmii:goto-tag tab)))
   (right-bar-click
    . ,(lambda (event button tab)
         (eval (nth 2 (assoc (string->symbol tab) status-bars)))))

   ;; Key Events

   ; Layout stuff
   ((key ,modkey "space")
    . ,(lambda _ (wmii:navigate-to "toggle")))
   ((key ,modkey "d")
    . ,(lambda _ (wmii:tag-settings-set! '(("colmode" . ("sel" "default"))))))
   ((key ,modkey "s")
    . ,(lambda _ (wmii:tag-settings-set! '(("colmode" . ("sel" "stack"))))))
   ((key ,modkey "m")
    . ,(lambda _ (wmii:tag-settings-set! '(("colmode" . ("sel" "max"))))))
   ((key ,modkey "Shift" "space")
    . ,(lambda _ (wmii:send-to "toggle")))
   ((key ,modkey "f")
    . ,(lambda _ (wmii:change-state "Fullscreen" 'toggle)))
   ((key ,modkey "Shift" "c")
    . ,(lambda _ (wmii:kill)))
   ((key ,modkey "escape")
    . ,(lambda _ (wmii:kill)))

   ; Actions
   ((key ,modkey "a")
    . ,(lambda _
         (and-let* ((action (wimenu `("reload" "rehash" "exec" "status" "quit"))))
           (cond
            ((string=? action "reload") (wmii:exec (format "wmii -r ~a/.wmii/wmiirc.scm" (getenv "HOME"))))
            ((string=? action "rehash") (update-programs))
            ((string-prefix? "exec " action) (wmii:exec (string-drop action 5)))
            ((string=? action "status") (status))
            ((string=? action "quit") (wmii:quit) (exit))))))

   ; Eval :D
   ((key ,modkey "Shift" "colon")
    . ,(lambda _
         (and-let* ((input (wimenu `(""))))
                   (wmii:write-tab "lbar" "eval"
                   (format "~s" (eval (with-input-from-string input read)))
                   wmii-evalcolors))))

   ; YubNub
   ((key ,modkey "Shift" "Return")
    . ,(lambda _
         (and-let* ((input (wimenu `(""))))
                   (fork-run (format "firefox 'http://www.yubnub.org/parser/parse?command=~a'" input)))))

   ; Other menus/running stuff
   ((key ,modkey "p")
    . ,(lambda _
         (and-let* ((program (wimenu programs))) (fork-run program))))
   ((key ,modkey "Return")
    . ,(lambda _ (fork-run wmii-term)))

   ; Tagging
   ((key ,modkey "t")
    . ,(lambda _
         (and-let* ((tag (wimenu (wmii:tags)))) (wmii:goto-tag tag))))
   ((key ,modkey "Shift" "t")
    . ,(lambda _
         (and-let* ((tag (wimenu (wmii:tags)))) (wmii:client-tags-set! (list tag)))))

   ;; Other key events
   ,@(map (lambda (x)
            `((key ,modkey ,(cdr x))
              . ,(lambda _ (wmii:navigate-to (->string (car x))))))
          directions)
   ,@(map (lambda (x)
            `((key ,modkey "Shift" ,(cdr x))
              . ,(lambda _ (wmii:send-to (->string (car x))))))
          directions)   
   ,@(map (lambda (x)
            `((key ,modkey ,(->string x))
              . ,(lambda _ (wmii:goto-tag x))))
          (iota 10))
   ,@(map (lambda (x)
            `((key ,modkey "Shift" ,(->string x))
              . ,(lambda _ (wmii:client-tags-set! (list (->string x)))))) (iota 10))))


;; Ready to run!

(let ((curtag (wmii:tag))) 
  (for-each (cut wmii:destroy-tab "lbar" <>) (wmii:tabs "lbar"))
  (for-each (lambda (t)
              (if (string=? curtag t)
                  (wmii:write-tab "lbar" t t wmii-focuscolors)
                  (wmii:write-tab "lbar" t t wmii-normcolors)))
            (wmii:tags)))

(status)
(wmii:event-loop)
