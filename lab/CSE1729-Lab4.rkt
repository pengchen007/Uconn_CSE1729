;;peng chen
;;Lab 004
;;2/21/2017

#lang racket
;problem1
(define (dominate f g)
  (define (helper-fun a b c)
  (if (> (a c) (b c))
      c
      (helper-fun a b (+ c 1))))
  (helper-fun f g 1))

;;test case
;;I define f(x)=3x^2-2  and g(x)=6x
(define (f x) (- (* 3 x x) 2))
(define (g x) (* 6 x))
(dominate f g)
;;when I run the test case, I get:
;3

;problem2
(define (double inc)
  (lambda (x) (inc(inc x))))
(define (inc x) (+ x 1))

;;test case
(((double (double double))inc)5)
;;when I run the test case, I get:
;;21


;problem 3
;a
(define (find sequence test n)
  (define (find-aux x found)
    (let* ((f1 (sequence x))
           (f2 (test f1)))
      (cond ((and f2 (= (+ found 1) n)) f1)
            (f2 (find-aux (+ x 1) (+ found 1)))
            (else (find-aux (+ x 1) found)))))
  (find-aux 1 0))
;b
(define (even x) (= (modulo x 2) 0))
(define (odd x) (not (even x)))
(define (se x) x)
;;test case
(find se even 5)
(find se odd 5)
;;when I run the test case, I get:
;;10
;;9

;c
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((>= n 1) (+ (fib (= n 1)) (fib (- n 2))))))
(define (divides a b) (= (modulo b a) 0))
(define (smooth n k)
  (and (>= k 2)
       (or (divides k n)
           (smooth n (- k 1)))))
(define (isprime? p)
  (not (smooth p (floor (sqrt p)))))
;;test case
(find se isprime? 5)
;;when I run the test case, I get:
;;7


;;problem4

(#%require plot)
(plot-new-window? #t)
(plot (function sin (- pi) pi #:label "sine␣x")
      #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)
(define (fu x) (+ (* (sin x) (- 3 x)) 1))
(plot (function fu)
      #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)



