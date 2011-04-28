(use posix numspell)

; Macros

(define-syntax add-event-handler
  (syntax-rules ()
    [(_ handler func)
     (wmii:event-handlers-set! (alist-update! 'handler func (wmii:event-handlers)))]))

(define-syntax add-key
  (syntax-rules ()
    [(_ keys body ...)
     (wmii:event-handlers-set! (alist-update! `(key ,modkey ,@keys) (lambda _ body ...) (wmii:event-handlers)))]))

(define-syntax add-setting
  (syntax-rules ()
    [(_ setting value)
     (wmii:global-settings-set! (alist-update! 'setting value (wmii:global-settings)))]))

(define file/read-lines (cut with-input-from-file <> read-lines))
(define (file/read-line file) (car (file/read-lines file)))

(define (number->english* number str)
  (string-titlecase
    (cond ((zero? number)
           (format "no ~as" str))
          ((eq? 1 number)
           (format "~a ~a" (number->english number) str))
          (else
            (format "~a ~as" (number->english number) str)))))

(define (fork-run . args)
  (process-wait (process-fork (lambda () (apply process-run args)))))

(define (shell args)
  (with-input-from-pipe args read-lines))

(define-syntax nth
  (syntax-rules ()
    ((nth i list)
     (list-ref list i))))

(define (wimenu options . rest)
  (receive (in out pid)
           (process "wimenu")
           (display (string-join options "\n") out)
           (close-output-port out)
           (let ((chosen (read-line in)))
             (close-input-port in)
             (and (string? chosen) chosen))))
