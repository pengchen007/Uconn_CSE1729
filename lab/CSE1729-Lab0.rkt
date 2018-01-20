(define (exchange-dollars-to-euros x) (* x 0.93))
(exchange-dollars-to-euros 50)

(define (exchange-euros-to-japanese-yuan x) (* x 121.73))
(exchange-euros-to-japanese-yuan 50)

(define (exchange-dollars-to-japanese-yuan x)(* x 0.93 121.73))
(exchange-dollars-to-japanese-yuan 50)

(define (det2x2 a b c d)(- (* a d) (* b c)))
(define (invert a b c d)
(if (= (det2x2 a b c d) 0)
     "This matrix is not ininvertible"
     "This matrix is invertible"))
(det2x2 -3 1 2 7)
(invert -3 1 2 7)
(det2x2 2 -4 -6 12)
(invert 2 -4 -6 12)

(define (det3x3 a b c
                d e f
                g h i)
(+(- (* a (det2x2 e f h i))
     (* b (det2x2 d f g i))
  (* c (det2x2 d e g h)))))
(det3x3 0 5 -6
        8 -11 4
        5 1 1)