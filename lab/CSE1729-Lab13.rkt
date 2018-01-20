#lang racket/base
;Peng Chen
;Lab 13
;4/25/2017
"problem 1"

(define (make-queue maxsize)
  (let ((new-queue (make-vector maxsize))
        (front 0)
        (back 0))
    (define (empty?) (eq? front back))
    (define (enqueue x)
      (vector-set! new-queue back x)
      (set! back (+ back 1)))
    (define (dequeue)
      (if (empty?)
          "error! called with an empty queue"
          (let ((m (vector-ref new-queue front)))
            (begin
              (vector-set! new-queue front 0)
              (set! front (+ front 1)))
            m)))
    (define (dispatcher method)
      (cond ((eq? method 'empty?) empty?)
            ((eq? method 'enqueue) enqueue)
            ((eq? method 'dequeue) dequeue)))
    dispatcher))

;test case
(define v (make-queue 2000000))
((v 'empty?))
((v 'enqueue) 8)
((v 'enqueue) 2)
((v 'enqueue) 1)
((v 'enqueue) 9)
((v 'enqueue) 6)
((v 'empty?))
((v 'dequeue))
((v 'dequeue))
((v 'dequeue))
((v 'dequeue))
((v 'dequeue))
((v 'dequeue))
((v 'empty?))
;when I run the test case I got
;#t
;#f
;8
;2
;1
;9
;6
;"error! called with an empty queue"
;#t





