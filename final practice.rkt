(+ 4 8 15 16 23 42)
(+ (* 653854321 241304201))
(/ (+ 5 4 (- 2
             (- 3
                (+ 6
                   (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

"problem 1"
"a"
(define (absolute x)
  (if (>= x 0)
      x
      (- 0 x)))
(absolute 5)
(absolute -6)
"b"
(define (convert-FtoC x)
  (* (/ 5 9) (- x 32)))
(convert-FtoC 68)
(define (convert-CtoF x)
  (+ (* (/ 9 5) x) 32))
(convert-CtoF 20)
"c"
(define (discount p d)
  (- p (* p d)))
(discount 100 0.1)
"d"
(define (tip p)
  (+ p (* p 0.15)))
(tip 100)
"e"
"problem 2"
"a"
(define (gcd1 a b)
  (if (= b 0)
      a
      (gcd1 b (remainder a b))))
(gcd1 252 105)
"problem 4"


"problem 5"
"a"
(define (slope-yintercept a b)
  (let ((m (/ (- (cdr b) (cdr a))
              (- (car b) (car a)))))
  (cons m
        (- (cdr a) (* (car a) m)))))
(slope-yintercept (cons 1 1) (cons -1 -1))

"b"
"problem 6"
"a"
(define (max-pair-dist L)
  (define (max-pair-dist-acc L max)
    (cond ((null? (cdr L)) max)
          ((> (absolute (- (car L) (cadr L))) max)
           (max-pair-dist-acc (cdr L) (absolute (- (car L) (cadr L)))))
          (else (max-pair-dist (cdr L)) max)))
  (max-pair-dist-acc L 0))

(max-pair-dist '(22 34 56 87 65))
"b"
(define (new-list l)
  (if (null? (cdr l))
      '()
      (cons (- (car (cdr l)) (car l))
            (new-list (cdr l)))))
(new-list '(1 8 5 7 1))

"c"
(define (show-range L)
  (define (find-min L)
    (if (null? (cdr L))
        (car L)
        (min (car L) (find-min (cdr L)))))
    (define (find-max L)
    (if (null? (cdr L))
        (car L)
        (max (car L) (find-max (cdr L)))))
  (cons (find-min L) (find-max L)))

(show-range '(1 8 5 7 1))
"d"

"problem 7"
(define (make-tree v left-tree right-tree)
  (list v left-tree right-tree))
(define (value T) (car T))
(define (left T) (cadr T))
(define (right T) (caddr T))
(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((eq? x (value T)) T)
        ((< x (value T)) (make-tree (value t)
                                    (insert x (left T))
                                    (right)))
        ((> x (value T)) (make-tree (value T)
                                    (left T)
                                    (insert x (right T))))))
(define T1 (make-tree 8
                      (make-tree 5
                                 (make-tree 3
                                            (make-tree 2
                                                       (make-tree 1
                                                                  '()
                                                                  '())
                                                       '())
                                            '())
                                 (make-tree 6
                                            '()
                                            '()))
                      (make-tree 10
                                 (make-tree 9
                                            '()
                                            '())
                                 (make-tree 11
                                            '()
                                            '()))))
(define T2
  (make-tree 6
             (make-tree 3
                        (make-tree 2
                                   (make-tree 1 '() '())
                                   (make-tree 3 '() '()))
                        (make-tree 5 '() '()))
             (make-tree 8
                        (make-tree 7
                                   '()
                                   (make-tree 8
                                              '()
                                              '()))
                        (make-tree 9
                                   '()
                                   '()))))
"a"
(define (count-one-child T)
  (cond ((null? T) 0)
        ((and (null? (left T)) (null? (right T))) 0)
        ((null? (left T)) (+ 1 (count-one-child (right T))))
        ((null? (right t)) (+ 1 (count-one-child (left T))))
        (else (+ (count-one-child (right T))
                 (count-one-child (left T))))))
(count-one-child T1)
"b"
(define (count v t)
  (cond ((null? t) 0)
        ((eq? v (value t)) (+ 1
                              (count v (right t))
                              (count v (left t))))
        (else (+ (count v (left t))
                 (count v (right t))))))
(count 3 T2)

"c"

"problem 8"
"a"
(define (listinsert L i n)
  (cond ))






