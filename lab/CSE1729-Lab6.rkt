;Peng Chen
;Lab 6
;2/28/2017

;problem 1
(define (p x) (cons x (* x x)))

;test case
(p 6)
;When I run the test case I got:
;(6 . 36)

;problem 2
(define (square-1 list)
  (if (null? list)
      '()
      (cons (* (car list) (car list))
            (square-1 (cdr list)))))

;test case
(square-1 '(1 2 3 4 5))
;When I run the test case I got:
;(1 4 9 16 25)

;problem 3
(define (range p)
  (define (helper x list)
    (if (> (car p) x)
        list
        (helper (- x 1) (cons x list))))
  (helper (cdr p) '()))

;test case
(range (cons 0 10))
;When I run the test case I got:
;(0 1 2 3 4 5 6 7 8 9 10)

;problem 4
(define (sv-mult a list)
  (if (null? list)
      '()
      (cons (* a (car list))
            (sv-mult a (cdr list)))))

;test case
(sv-mult 3 '(1 2 3 4 5))
;When I run the test case I got:
;((3 6 9 12 15)

;problem 5
(define (v-add list1 list2)
  (if (null? list1)
      '()
       (cons (+ (car list1) (car list2))
             (v-add (cdr list1) (cdr list2)))))

;test case
 (v-add '(1 2 3) '(4 5 6))
;When I run the test case I got:
;(5 7 9)

;problem 6
(define (dot x y)
  (if (null? x)
      0
      (+ (* (car x) (car y))
         (dot (cdr x) (cdr y)))))

;test case
(dot '(1 2 3) '(4 5 6))
;When I run the test case I got:
;32

;problem 7
(define (cross x y)
  (define (helper a l)
  (if (null? l)
      '()
      (cons (cons a (car l))
            (helper a (cdr l)))))
  (if (null? x)
      '()
      (append (helper (car x) y) (cross (cdr x) y))))

;test case      
(cross '(1 2 3) '(4 5 6))
;When I run the test case I got:
;((1 . 4) (1 . 5) (1 . 6) (2 . 4) (2 . 5) (2 . 6) (3 . 4) (3 . 5) (3 . 6))