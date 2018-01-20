;Peng Chen
;Problem set 5
;3/6/2017

;problem 1
(define f (lambda(x) (- x)))
(define g (lambda(x) x))
(define (max-x p)
  (let ((f (car p))
        (g (cdr p)))
  (define (m x)
    (if (< (f x) (g x))
        (g x)
        (f x)))
  m))
(define mymax (max-x (cons f g)))
(mymax 9)
;When I run the test case I got:
;9


;problem 2
(define (zip list1 list2)
  (if (null? list1)
      '()
      (cons (cons (car list1) (car list2))
            (zip (cdr list1) (cdr list2)))))
;test case
(zip '(1 2 3 4 5) '(1 2 3 4 5))
;When I run the test case I got:
;((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5))

;problem 3
(define (unzip l)
  (define (help1 l1)
    (if (null? l1)
        '()
        (cons (car (car l1))
              (help1 (cdr l1)))))
  (define (help2 l2)
    (if (null? l2)
        '()
        (cons (cdr (car l2))
              (help2 (cdr l2)))))
  (list (help1 l) (help2 l)))

;test case
(unzip '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6)))
;When I run the test case I got:
;((1 2 3 4 5) (2 3 4 5 6))


;problem 4
(define (change k l)
  (cond ((= k 0) 1)
        ((or (< k 0) (null? l)) 0)
        (else
         (+ (change k (cdr l))
            (change (- k (car l)) l)))))

;test case
(change 11 '(1 5 10 25))
;When I run the test case I got:
;4


;problem 5
;a
(define (encode p)
  (+ (/ (* (+ (car p) (cdr p)) (+ (car p) (cdr p) 1))
        2)
     (cdr p)))

;test case
(encode (cons 6 8))
;When I run the test case I got:
;113


;b
(define (decode z)
  (let* ((w (/ (- (sqrt (+ (* 8 z) 1)) 1) 2))
         (t (/ (+ (* w w) w) 2))
         (y (floor (- z t)))
         (x (floor (- w y))))
  (cons x y)))

;test case
(decode 8)
;When I run the test case I got:
;(3.0 . 0.0)

;problem 6
(define (positives l)
  (if (null? l)
      '()
      (if (<= (car l) 0)
          (positives (cdr l))
          (cons (car l)
                (positives (cdr l))))))

;test case
(positives '(-9 2 4 6 -8 0 4 -6 5 8 5))
;When I run the test case I got:
;(2 4 6 4 5 8 5)

;problem 7
(define (remove-par d l)
  (cond ((null? l) '())
        ((d (car l))
         (cons (car l)
               (remove-par d (cdr l))))
        (else (remove-par d (cdr l)))))
(define (del-dup a)
  (if (null? a)
      '()
      (cons (car a)
            (del-dup (remove-par (lambda (x) (not (equal? x (car a))))
                                 (cdr a))))))

;test case
(del-dup '(1 3 4 5 3 6 3 6 3 3 3 3 6 5 3 4 4 3 ))
;When I run the test case I got:
;(1 3 4 5 6)


