

(define (power a b)
  (if (= b 0)
      1
      (* a (power a (- b 1)))))
(power 2 3)

(define (power-tri a b)
  (define (power-accumulate a b c)
    (if (= b 0)
        c
        (power-accumulate a (- b 1) (* a c))))
  (power-accumulate a b 1))
(power-tri 2 4)