;;Peng Chen
;;CSE1729 
;;2/13/2017
"problem 1"
(define (Hn n)
  (if (= n 0)
      0
      (+ (* 1.0 (/ 1 n)) (Hn (- n 1)))))
(define (euler-constant n) (- (Hn n) (log n)))
;;test case
(euler-constant 3000)
;;When I run the test case I get:
;;0.5773823223089227

"problem 2"
(define (prime? n)
  (define (divisor? k) (= 0 (modulo n k)))
  (define (divisors-upto k)
    (and (> k 1)
         (or (divisor? k) (divisors-upto (- k 1)))))
  (not (divisors-upto (- n 1))))
(define (count-prime m)
  (cond ((= m 1) 0)
        ((prime? m) (+ (count-prime (- m 1)) 1))
        ((count-prime (- m 1)))))
;;test case
(count-prime 20)
;;When I run the test case I get:
;;8

"problem 3"
;;(a)
(define (Ln n)
  (cond ((= n 0) 2)
        ((= n 1) 1)
        ((> n 1) (+ (Ln (- n 1)) (Ln (- n 2))))))

;;(b)
(define (Lucas-ratio n) (/ (* 1.0 (Ln n)) (Ln (- n 1))))
;;test case
(Lucas-ratio 20)
(Lucas-ratio 21)
(Lucas-ratio 22)
(Lucas-ratio 23)
;;When I run the test case I get:
;;1.6180340143330838
;;1.6180339789779863
;;1.6180339924824318
;;1.6180339873241927

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((> n 1) (+ (fib (- n 1))
                    (fib (- n 2))))))
(define (fibonacci-ratio n) (/ (* 1.0 (fib n)) (fib (- n 1))))

;;test case
(fibonacci-ratio 20)
(fibonacci-ratio 21)
(fibonacci-ratio 22)
(fibonacci-ratio 23)
;;When I run the test case I get:
;;1.6180339631667064
;;1.6180339985218033
;;1.618033985017358
;;1.6180339901755971
;;I notice the result is very close each other

;;(c)
(define (fast-Lucas-help n k Luc-a Luc-b)
  (if (= n k)
      Luc-a
      (fast-Lucas-help n (+ k 1) (+ Luc-a Luc-b) Luc-a)))
(define (fast-Lucas n) (fast-Lucas-help n 1 1 2))
;;test case
(fast-Lucas 3)
(fast-Lucas 4)
(fast-Lucas 5)
(fast-Lucas 6)
(fast-Lucas 50)
;;When I run the test case I get:
;;4
;;7
;;11
;;18
;;28143753123
;;There are 4 8 14 22 recursive calls made by lucas function
;;there are 2 3 4 5 recursive calls made by fast-lucas dunction


"problem 4"
"a"
(define (golden-ratio n)
  (if (= n 1)
      2
      (+ 1 (/ 1.0 (golden-ratio (- n 1))))))

;;test case
(golden-ratio 5)
(golden-ratio 8)
(golden-ratio 15)
(golden-ratio 20)
;;When I run the test case I get:
;;1.625
;;1.6176470588235294
;;1.618034447821682
;;1.618033985017358

"b"
(define (golden-ratio2 n)
  (if (= n 1)
      (sqrt 1)
      (sqrt (+ 1 (golden-ratio2 (- n 1))))))
;;test case
(golden-ratio2 8)
(golden-ratio2 15)
(golden-ratio2 20)
(golden-ratio2 30)
(golden-ratio2 100)
;;When I run the test case I get:
;;1.617851290609675
;;1.6180339395887897
;;1.6180339886113682
;;1.6180339887498938
;;1.618033988749895
"problem 5"
(#%require(only racket/base random))
(define (one-sample)
  (let* ((x (- (* 2 (random)) 1))
         (y (- (* 2 (random)) 1))
         (r (sqrt (+ (* x x) (* y y)))))
  (<= r 1)))
(define (count-fall k)
  (cond ((= k 0) 0)
        ((one-sample) (+ 1 (count-fall (- k 1))))
        ((count-fall (- k 1)))))
(define (Pi k) (* 4.0 (/ (count-fall k) k)))
;;test case
(Pi 10000)
;;When I run the test case I get:
;;3.1664


"problem 6"
;;this question is a free response question
;;if m-n is a odd number then it will be work
;;if m-n is a even number the m and n will never be equal so the caculate is no terminated
