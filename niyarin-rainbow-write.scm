(define-library (niyarin rainbow write)
   (import (scheme base)
           (srfi 1)
           (scheme write))

   (export write-rainbow display-rainbow)

   (begin
     (define color-list
        (circular-list
            "\x1b[31m"
            "\x1b[32m"
            "\x1b[33m"
            "\x1b[34m"
            "\x1b[35m"
            "\x1b[36m"))

     (define default-color
        "\x1b[39m")

     (define (write-display-rainbow obj write-display . opt-port)
       (let ((port (if (null? opt-port) (current-input-port) (car opt-port))))
         (let loop ((obj obj)
                    (color-list color-list))
           (cond
             ((and (list? obj) (not (null?  obj)))
                 (display (car color-list))
                 (display "(")
                 (display default-color)


                 (let loop2 ((ls obj))
                   (cond
                     ((null? (cdr ls))
                        (loop (car ls) (cdr color-list)))
                     (else
                       (loop (car ls) (cdr color-list))
                       (display " ")
                       (loop2 (cdr ls)))))

                 (display (car color-list))
                 (display ")")
                 (display default-color))

             ((vector? obj)
                 (display "#(")

                  (unless (zero? (vector-length obj))
                     (let loop2 ((index 0))
                        (if (= index (- (vector-length obj) 1))
                          (loop (vector-ref obj index) color-list)
                          (begin
                             (loop (vector-ref obj index) color-list)
                             (loop2 (+ index 1))))))

                 (display ")"))
             (else
               (write-display obj))))))

      (define (write-rainbow obj . opt-port)
       (let ((port (if (null? opt-port) (current-input-port) (car opt-port))))
         (write-display-rainbow obj write port)))

      (define (display-rainbow obj . opt-port)
       (let ((port (if (null? opt-port) (current-input-port) (car opt-port))))
         (write-display-rainbow obj display port)))
     ))

;example
;
;(import (scheme base)
;        (scheme write)
;        (niyarin rainbow write))
;
;(display-rainbow '((list (list (list (list (list (list (list "123" "456")))))))))
;(newline)
