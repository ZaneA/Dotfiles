#!/usr/bin/csi -s

;;; Zanes wmii config, in chicken scheme
;;; Modified from the wmii egg example

(require-library wmiirc srfi-18 posix regex regex-literals srfi-13)
(import scheme chicken (prefix wmiirc wmii:) srfi-18 posix regex regex-literals srfi-13)


(wmii:connect)


;;; Main config

(define modkey "Mod4")

(define directions
  `((up    . "k")
    (down  . "j")
    (left  . "h")
    (right . "l")))

(define wmii-normcolors  '(#xf6f3e8 #x242424 #x303030))
(define wmii-focuscolors '(#x000000 #xcae682 #x303030))
(define wmii-font "-*-terminus-*-*-*-*-*-*-*-*-*-*-*-*")
(define wmii-colmode "default")
(define wmii-barposition "on bottom")
(define wmii-floatborder "3")
(define wmii-term "urxvt")

(define wmii-evalcolors '(#xaa8888 #x242424 #x242424))

(define fork-run #f) ; Forward decl
(define shell #f) ; Forward decl

(define status-bars
  `((b-dropbox (conc "Dropbox: " (string-join (shell "dropbox status")))
               (fork-run (conc wmii-term " -cd ~/Dropbox"))
               (#xcc8888 #x242424 #x242424))
    (a-music (string-join (shell "nyxmms2 status -f '${artist} - ${title}'"))
             (fork-run "abraca") ; Eval'd on click
             (#x88cc88 #x242424 #x242424))
    (c-loadavg (string-join (shell "awk '{print $1\" \"$2\" \"$3}' /proc/loadavg"))
               (fork-run (conc wmii-term " -e htop"))
               (#x8888cc #x242424 #x242424))
    (d-date (string-join (shell "date '+%A %e %B %l.%M%P'"))
            #f
            (#xcccc88 #x242424 #x242424))))


;;; Rule config

(wmii:colrules-set!
 `(("gimp"   . (17 83 41))
   (".*"     . (62 38))))

(wmii:tagrules-set!
 `(("Quod Libet|Abraca"  . "1+Music")
   ("MPlayer|VLC|Totem"  . ("1+Video" "~"))
   ("Emacs"              . "1+Emacs")
   ("Firefox|Vimperator" . "1+Web")
   ("urxvt"              . "1+Term")
   ("gimp"               . "Graphics")
   (".*"                 . "sel")
   (".*"                 . "1")))

(wmii:global-settings-set!
 `((font        . ,wmii-font)
   (focuscolors . ,wmii-focuscolors)
   (normcolors  . ,wmii-normcolors)
   (grabmod     . ,modkey)
   (colmode     . ,wmii-colmode)
   (bar         . ,wmii-barposition)
   (border      . ,wmii-floatborder)))


;;; Function definitions

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

; Run macro using above shell function
(define-syntax run
  (syntax-rules (as on)
    [(run command as username on machine)
     (let ([matches (string-match #/(.*?):(.+)/ machine)])
       (if (list? matches)
           (shell (format "ssh -C -p ~a ~a@~a ~a"
                           (nth 2 matches) ; Port
                           username
                           (nth 1 matches) ;Machine
                           command))))]
    [(run command on machine)
     (run command as (getenv "USERNAME") on machine)]
    [(run command)
     (shell command)]))

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
		  (thread-sleep! 1)
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
                   (fork-run (conc "x-www-browser 'http://www.yubnub.org/parser/parse?command=" input "'")))))

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
