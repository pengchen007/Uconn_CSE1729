"1 a"
(define (sum-of-first-n-positive-number n)
  (define (sequence n) (* n n))
  (if (= n 0)
      0
      (+  (sum-of-first-n-positive-number (- n 1)) (sequence n))))
(sum-of-first-n-positive-number 4)


"1 b"
(define (sum-of-first-n-even-number n)
  (define (squence n) (* 2 n))
  (if (= n 0)
      0
      (+ (sum-of-first-n-even-number (- n 1)) (squence n))))
(sum-of-first-n-even-number 1)
(sum-of-first-n-even-number 2)
(sum-of-first-n-even-number 3)
(sum-of-first-n-even-number 4)
(sum-of-first-n-even-number 5)
(sum-of-first-n-even-number 6)
(sum-of-first-n-even-number 7)
"this sequence numbers looks like n^2+n"


"2"
(define (recursive k)
  (if (= k 1)
      (/ 1 2)
      (* (recursive (- k 1))
         (- 1 (/ 1 (+ k 1))))))
(recursive 6)


"3"
(define (devisors n)
  (define (divides a b)
    (= 0 (modulo b a)))
  (define (divisors-upon n k)
    (cond ((= k 0) 0)
          ((= n 1) 0)
          ((= k 1) 1)
          ((divides k n) (+ 1 (divisors-upon n (- k 1))))
          (else (divisors-upon n (- k 1)))))
  (divisors-upon n n))
(devisors 4)
(devisors 5)
(devisors 10)



"4"
(define (sum-of-first-k-terms-infinite-series k)
  (if (= k 0)
      0
      (+ (sum-of-first-k-terms-infinite-series (- k 1))
         (* (expt -1 (- k 1))
            (/ 4.0
               (- (* 2 k) 1))))))

(sum-of-first-k-terms-infinite-series 100)
(sum-of-first-k-terms-infinite-series 100000)
"This number is very close to Pi"


"5 a"
(sum-of-first-k-terms-infinite-series 300)
"To compute the 300 terms, made 300 calls to expt, odd calls passed the 1, even calls passed the - 1"

"b"
(define (first-k-terms-sum k)
  (define (divides a b) (= 0 (modulo b a)))
  (cond ((= k 1) 4)
        ((divides 2 k) (+ (/ -4.0 (- (* 2 k) 1))
                          (first-k-terms-sum (- k 1))))
        (else (+ (/ 4.0 (- (* 2 k) 1))
                 (first-k-terms-sum (- k 1))))))

(first-k-terms-sum 100)


"6"

"if replace the if with new-if, the program run out of memory and interaction disabled"


"7"
(define (new-sin x n)
  (define (factorial n)
    (if (= n 0)
        1
        (* n (factorial (- n 1)))
    )
  )
  (if (= n 0)
      x
      (+ (new-sin x (- n 1))
         (/ (* (expt -1 n)
               (expt x (+ (* 2 n) 1)))
            (factorial (+ (* 2 n) 1)))
       )
   )
)

(new-sin 0.523 1)
(new-sin 0.785 5)
(new-sin 1.047 20)


