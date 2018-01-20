
"1 (a)"
(* (+ 22 42)
   (* 54 99))

"(b)"
(* (* (+ 22 42)
      54)
   99)

"(c)"
(+ (* 62 102)
   (* 16
      (/ 44 22)))

"(d)"
"The operation order is different, (a) evaluates the sum of the 22 and 42 equal 64, then caculate the product of 54 and 99 equal 5346, then caculate the product of 64 and 5346 equal 342144"
"(b) cuculate the sum of 22 and 42 equal 64, then caculate the product of 64 and 54 equal 3456, then caculate the product of 3456 and 99 equal 342144"

"(e)"
"not necessary for evaluating arithmetic expressions in SCHEME"

"2"
"(a)(22 42 +) (54 99 *) *"
"(b)((22 42 +) 54 *) 99 *"
"(c)((64 102 *) 16 +) (44 22 /) *"

"3"
"a"
(define (inc x) (+ x 1))
(define (inc2 x) (inc (inc x)))
(inc2 3)

"b"
(define (square x) (* x x))
(define (fourth x) (square (square x)))
(fourth 2)

"c"
(define (a x ) (+ (square x) 1))
(define (b x) (+ (* (fourth x) 16) 22))
(define (p x) (* (fourth (a x)) (square (b x))))
(p 1)

"d"
(define (sixteenth x) (fourth (fourth x)))
(sixteenth 1.01)
(define (sixty-fourth x) (fourth (sixteenth x)))
(sixty-fourth 1.01)

"e"
"the difficulity of use * to define sixty-fourth is you have to type x 64 times"

"4"
"a"
(define (normal x sig)
  (/ (exp (/ (* -1 (square x)) (* 2 (square sig))))
  (sqrt (* 2 3.142 (square sig)))))
(normal 0 1)
(normal 0 2)

"b"
(define (fspiral theta)
  (expt 1.618 (* theta (/ 2 3.142))))
(fspiral 30)

"c"
(define (malth t p a)
  (* p (expt 2 (* a t))))
(malth 1 2 3)
(malth 2 0 6)

"d"
(define (singlespecies Pi Ps alpha t)
  (/ (* Ps Pi)
     (+ Pi
        (* (- Ps Pi)
           (exp (* -1 alpha t))))))
(singlespecies 10 1000 0.04 50)
(singlespecies 10 1000 0.04 100)
(singlespecies 10 1000 0.04 200)
(singlespecies 10 1000 0.04 500)
(singlespecies 10 1000 0.04 1000)
(singlespecies 10 1000 0.04 2000)
