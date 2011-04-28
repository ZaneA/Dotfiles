;;
;; Fancy Status Bar
;;

(define *status-bar-interval* 2)

(define status-bars
  `((a-email 
      (let ((claws-status (nth 1 (string-split (car (shell "claws-mail --status"))))))
        (string-append
          "Mail: "
          (number->english*
            (if (not (string=? "Claws" claws-status))
              (string->number claws-status)
              0)
            "message")))
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

(add-event-handler right-bar-click (lambda (event button tab) (eval (nth 2 (assoc (string->symbol tab) status-bars)))))

(status)
