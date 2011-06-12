;;
;; Fancy Status Bar
;;

(define *status-bar-interval* 2)
(define status-bars (list))
(define status-count 0)

(define (add-status-bar #!key (bar "rbar") (text status-count) (action #f) (color '(#x444444 #x000000 #x000000)))
  (set! status-bars
    (append
      (list (list (begin
                    (set! status-count (+ status-count 1))
                    (format "z-~a" status-count))
                  text action color bar))
      status-bars)))

(add-status-bar
  text: '(let ((claws-status (nth 1 (string-split (car (shell "claws-mail --status"))))))
           (string-append
             "Mail: "
             (number->english*
               (if (not (string=? "Claws" claws-status))
                 (string->number claws-status)
                 0)
               "message")))
  color: '(#xffffff #x000000 #x000000))

(add-status-bar text: '(car (shell "conky -c ~/.wmii/conky")) color: '(#x666666 #x000000 #x000000))

(add-status-bar
  text: '(let ((now-playing (shell "ncmpcpp --now-playing")))
           (if (> (length now-playing) 0) (car now-playing) "None"))
  action: '(fork-run (conc wmii-term " -e ncmpcpp"))
  color: '(#xcb8cff #x000000 #x000000))

(add-status-bar
  text: '(time->string (seconds->local-time) "%A %e %B %l.%M%P")
  color: '(#xccdd88 #x000000 #x000000))


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
                     (wmii:write-tab (nth 4 bar) (nth 0 bar) (string-trim-both (eval (nth 1 bar))) (nth 3 bar)))
                   (append status-bars `((0 "" #f ,wmii-normcolors "rbar"))))
		  (thread-sleep! *status-bar-interval*)
		  (loop)))))))))

(add-event-handler right-bar-click (lambda (event button tab) (eval (nth 2 (assoc tab status-bars)))))

(status)
