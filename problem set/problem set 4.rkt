;Peng Chen
;Peoblem set 4
;2/27/2017


;;problem 1
(define (har n)
  (define (f n)
    (if (= n 0)
        0
        (/ 1.0 n)))
  (define (sum f n)
  (if (= n 0)
      (f 0)
      (+ (f n) (sum f (- n 1)))))
  (sum f n))

;;test case
(har 8)
;;when I run this test case, I got:
;;2.9289682539682538


;;problem 2
;;a
(define (der f h)
  (lambda (x) (/ (- (f (+ x h)) (f x)) h)))

;;test case
(define (f x) (+ x 1))
((der f 0.01) 2)

;;b
(define (app-der f h n)
  (if (= n 0)
      f
      (app-der (der f h) h (- n 1))))

;;test case
(define (g x) (* x x x))
((app-der (der g 0.01) 0.01 3) 3)

;;2c

(#%require plot)
(plot (function (der sin 0.5) (- pi) pi #:label "sine x"))
(plot (function cos(- pi) pi #:label "cos"))





;;problem 3
;;a
(define (newton-method f x0 n)
  (if (= n 0)
      x0
      (- (newton-method f x0 (- n 1)) (/ (f (newton-method f x0 (- n 1)))
                                         ((der f 0.01) (newton-method f x0 (- n 1)))))))
(define (f x) (+ (* 2 x) 1))
;;test case
(newton-method f 2 5)
;;when I run this test case, I got:
;;-0.5

;;b
(define (f1 x) (- (expt x 2) x 1))
;;test case
(newton-method f1 2 5)
;;when I run this test case, I got:
;;1.6180340146045318

;;problem 4
(define (sqrt-newt n)
  (define (h x)
    (- (expt x 2) n))
  (newton-method h 1 5))

;;test case
(sqrt-newt 40)
;;when I run this test case, I got:
;;6.325070514192499


;;problem 5

(define (simsrule f a b n)
  (define (sum term a b)
  (if (> a b)
      0(+ (term a)
      (sum term (+ a 1) b))))
  (define h (/ (- b a) n))
  (define (g k) (f (+ a (* k h))))
  (define (inc x) (+ x 1))
  (* g h))
(define (f x) x)
(simsrule f 0 1 30)






