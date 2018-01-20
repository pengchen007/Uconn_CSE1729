#lang racket
;Peng Chen
;CSE1729 problem set6
;3/4/2017

;problem 1
;a
(define (sum-l l)
  (if (null? l)
      0
      (+ (car l) (sum-l (cdr l)))))
;test case
(sum-l '(1 2 3 4))
;When I run the test case I got:
;10

;b
(define (average-l l)
  (if (null? l)
      0
      (/ (sum-l l) (length l))))
;test case
(average-l '(1 2 3 4 5))
;When I run the test case I got:
;3

;c
(define (square-dev l)
  (map (lambda (x) (expt (- x (average-l l)) 2)) l))
;test case
(square-dev '(1 2 3 4 5))
;When I run the test case I got:
;(4 1 0 1 4)

;d
(define (standard-dev l)
  (sqrt (average-l (square-dev l))))
;test case
(standard-dev '(1 2 3 4 5))
;When I run the test case I got:
;1.4142135623730951

;e
(define (map2 f l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (f (car l1) (car l2))
            (map2 f (cdr l1) (cdr l2)))))
;test case
(map2 (lambda (x y) (+ (* x x) (* y y))) '(1 2 3 4) '(1 2 3 4))
;When I run the test case I got:
;(2 8 18 32)

;f
(define (covarianve-l l1 l2)
  (map2 (lambda (x y) (* (- x (average-l l1)) (- y (average-l l2)))) l1 l2))
;test case 
(covarianve-l '(1 3 5 7 ) '(2 4 6 8))
;When I run the test case I got:
;(9 1 1 9)

;g
(define (pearson l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (/ (average-l (covarianve-l l1 l2))
         (* (standard-dev l1) (standard-dev l2)))))
;test case
(pearson '(1 2 3 4 5) '(1 2 3 4 5))
;When I run the test case I got:
;0.9999999999999998

;problem 2
;a
(define (best-fit X Y)
  (let* ((r (pearson X Y))
         (b (* r (/ (standard-dev Y) (standard-dev X)))))
    (cons (- (average-l Y) (* b (average-l X))) b)))

;test case
(best-fit '(1 3 5 7) '(2 4 6 8))
;When I run the test case I got:
;(1.0000000000000009 . 0.9999999999999998)

;b
(define (best-fit-fu pX pY)
  (let* ((p (best-fit pX pY))
         (a (car p))
         (b (cdr p)))
    (lambda (x) (+ (* b x) a))))

(define X '(160 180 200 220 240 260 280))
(define Y '(126 103 82 75 78 40 20))
(define fitline (best-fit-fu X Y))

;c
(define (zip2 x y)
  (map2 vector x y))

(require plot)
(plot-new-window? #t)
(plot (list (axes)
            (function fitline 140 300)
            (points (zip2 X Y))))

;problem 3
(define (merge l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((>= (car l1) (car l2))
         (cons (car l2) (merge l1 (cdr l2))))
        ((>= (car l2) (car l1))
         (cons (car l1) (merge (cdr l1) l2)))))

;teat case
(merge '(1 3 5 7 9) '(2 4 6 8 10))
(merge '(1 3 6 7) '(2 4 8 9 10))
;When I run the test case I got:
;(1 2 3 4 5 6 7 8 9 10)
;(1 2 3 4 6 7 8 9 10
