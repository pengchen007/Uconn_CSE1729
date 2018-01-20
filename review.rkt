;;1
(define (energy-from-mass m)
  (let ((c 199792458))
  (* c c m)))
(energy-from-mass 1)
;;2a
(define (help-f n)
    (if (= n 1)
        (/ 1 10)
        (* (/ 1 10) (help-f (- n 1)))))
(define (arb-num n)
  (if (= n 1)
      (/ 1 10)
      (+ (help-f n) (arb-num (- n 1)))))
(arb-num 2)

;;2b
(define (f n)
  (if (even? n) 4
      5))
(f 10)

;;2c
(define (total-block n)
  (if (= n 0)
      0
      (+ (* (- (* 2 n) 1) (- (* 2 n) 1)) (total-block (- n 1)))))
(total-block 1)
(total-block 2)
(total-block 3)
(total-block 4)
;;3
(define (pt t)
  (cond ((= t 0) 2)
        ((< (pt (- t 1)) 10000) (* (pt (- t 1)) (pt (- t 1))))
        ((>= (pt (- t 1)) 10000) (* (pt (- t 1)) 2))))
(pt 5)

;;6
(define (two-power n)
  (if (= n 0)
      1
      (* 2 (two-power (- n 1)))))
(two-power 3)

;;7


(define (product f n)
  (define (f n)
  (if (= n 0)
      1
      n))
  (if (= n 0)
      (f 0)
      (* (f n) (product f (- n 1)))))
(product f 6)


(define (odd-double-factorial n)
  (define (f n)
    (if (even? n)
        1
        n))
  (if (= n 1)
      1
      (* (f n) (odd-double-factorial (- n 1)))))

(odd-double-factorial 9)

