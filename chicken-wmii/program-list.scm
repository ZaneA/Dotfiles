;;
;; Build list of programs to be displayed in menu
;;

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

(add-key '("p") (and-let* ((program (wimenu programs))) (fork-run program)))
