#lang racket
;Peng Chen
;Lab 10
;4/4/2017

"problem 1"
(define (num->list n)
  (define (num->listhelper n l)
    (cond ((< n 10) (cons n l))
          (else (num->listhelper (floor (/ n 10)) (cons (modulo n 10) l)))))
  (num->listhelper n '()))

;test case
(num->list 12345)
;When I run the test case I got:
;'(1 2 3 4 5)

"problem 2"
(define (list->num l)
  (define (list->numhelper l n)
    (cond ((null? l) 0)
          (else (+ (* (car l) (expt 10 (- (length l) 1)))
                   (list->numhelper (cdr l) n)))))
  (list->numhelper l 0))
;test case
(list->num '(1 2 3 4 5))
;When I run the test case I got:
;12345

"problem 3"
"b"
(define (sum-digits l)
  (if (null? l)
      0
      (+ (expt (car l) 2) (sum-digits (cdr l)))))
;test case
(sum-digits '(1 2 3))
;When I run the test case I got:
;14

"c"
(define (make-tree value left right)
  (list value left right))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))

(define (element? x T)
  (cond ((null? T) #f)
        ((eq? x (value T)) #t)
        ((< x (value T)) (element? x (left T)))
        ((> x (value T)) (element? x (right T)))))

(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((eq? x (value T)) T)
        ((< x (value T)) (make-tree (value T)
                                    (insert x (left T))
                                    (right T)))
        ((> x (value T)) (make-tree (value T)
                                    (left T)
                                    (insert x (right T))))))

"d"
(define (is-happy? x)
  (define (is-happy-aux x s)
    (let ((sum (sum-digits (num->list x))))
      (cond ((= sum 1) #t)
            ((element? sum s) #f)
            (else (is-happy-aux sum (insert sum s))))))
  (is-happy-aux x '()))
;test case
(is-happy? 5)
(is-happy? 7)
;When I run the test case I got:
;#f
;#t

"e"
(define (prime n)
  (define (divides a b) (= (modulo b a) 0))
  (define (smooth k)
    (and (>= k 2)
         (or (divides k n)
             (smooth (- k 1)))))
  (not (smooth (floor (sqrt n)))))

(define (happy-prime? n)
  (if (and (prime n) (is-happy? n))
      #t
      #f))


(happy-prime? 7)
(happy-prime? 6)
;When I run the test case I got:
;#t
;#f

"f"
(define (find sequence test n)
  (define (find-aux x found)
    (let* ((fx ( sequence x))
           (satisfies-test (test fx)))
      (cond ((and satisfies-test
                  (= (+ found 1) n))
             fx)
            (satisfies-test (find-aux (+ x 1) (+ found 1)))
            (else (find-aux (+ x 1) found)))))
  (find-aux 1 0))

(define (happy-primes n)
  (define (happy-primes-aux x)
    (if (> x n)
        '()
        (cons (find identity happy-prime? x)
              (happy-primes-aux (+ x 1)))))
  (happy-primes-aux 1))

;test case
(happy-primes 21)
;When I run the test case I got:
;'(1 7 13 19 23 31 79 97 103 109 139 167 193 239 263 293 313 331 367 379 383)
;so 21th happy prime is 383

  
             
         



