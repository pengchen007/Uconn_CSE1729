"1"
"a"
(define (ancestors-in-nth-previous-generation n)
  (if (= n 1)
      2
      (* 2 (ancestors-in-nth-previous-generation (- n 1)))))
(ancestors-in-nth-previous-generation 5)

"b"
(define (Total-number-of-ancentors n)
  (if (= n 0)
      0
      (+ (ancestors-in-nth-previous-generation n) (Total-number-of-ancentors (- n 1)))))
(Total-number-of-ancentors 5)

"2"
"a"
( define ( pi-approx k)
   ( define ( factorial k)
      (if (= k 0)
          1
          (* k ( factorial (- k 1)))))
   ( define ( pi-aux k)  
      (if (< k 0)
          0
          (+ (/ (* ( factorial (* 4 k))
                   (+ 1103 (* 26390 k) ))
                (* (expt (factorial k) 4)
                   (expt 396 (* 4 k))))
             (pi-aux (- k 1)))))   
   (/ 1 (* (/ (* 2 ( sqrt 2))
              9801)
           ( pi-aux k)))
)
( pi-approx 0)
( pi-approx 1)

"b"
(define (pell-num n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((+ (* 2 (pell-num (- n 1))) (pell-num (- n 2))))))
"c"
(define (comp-pell-num n)
  (cond ((= n 0) 2)
        ((= n 1) 2)
        ((+ (* 2 (comp-pell-num (- n 1))) (comp-pell-num (- n 2))))))
"d"
(define (sqrt2-approx n)
  (/ (comp-pell-num n) (pell-num n) 2))
(sqrt2-approx 6)



"3"
(define (new-sqrt x n)
  (define (fn n)
    (cond ((= n 1) (/ (- x 1) 2))
          ((/ (- x 1) (+ 2 (fn (- n 1)))))))
  (+ 1 (fn n)))
(new-sqrt 4 5)
(new-sqrt 4 20)
(new-sqrt 9 5)
(new-sqrt 9 20)


