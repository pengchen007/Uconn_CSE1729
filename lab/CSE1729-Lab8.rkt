;Peng Chen
;Lab 8
;3/21/2017

(define (make-tree value left right)
  (list value left right))


(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))


(define (insert x T)
  (cond ((null? T) (make-tree x '() '()))
        ((eq? x (value T)) T)
        ((< x (value T)) (make-tree (value T)
                                    (insert x (left T))
                                    (right T)))
        ((> x (value T)) (make-tree (value T)
                                    (left T)
                                    (insert x (right T))))))


(define T1
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
;problem 1

(define (tree-depth T)
  (if (null? T) 0
        (let ((left_d (tree-depth (left T))) (right_d (tree-depth (right T))))
        (if (> left_d right_d) (+ 1 left_d)
             (+ 1 right_d)))))
;test case
(tree-depth T1)
;When I run the test case I got:
;4

;problem 2
(define (occurences-in-tree v T)
  (define (value-at-root? T)
    (if (eq? (car T) v) 1 0))
  (if (null? T)
      0
      (+ (value-at-root? T)
         (occurences-in-tree v (left T))
         (occurences-in-tree v (right T)))))

;test case
(occurences-in-tree 3 T1)
;When I run the test case I got:
;2

(define T2 (make-tree 8
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

;problem 3
(define (count-one-child T)
  (cond ((null? T) 0)
        ((and (null? (left T)) (null? (right T))) 0)
        ((null? (left T))
         (+ 1 (count-one-child (right T))))
        ((null? (right T))
         (+ 1 (count-one-child (left T))))
        (else (+ (count-one-child (left T))
                 (count-one-child (right T))))))

;test case
(count-one-child T2)
;When I run the test case I got:
;2             

;problem 4
(define (invert-bst T)
  (cond ((null? T) T)
        ((and (null? (left T)) (null? (right T))) T)
        ((make-tree (value T) (invert-bst (right T)) (invert-bst (left T))))))

;test case
(invert-bst T2)
;When I run the test case I got:
;(8 (10 (11 () ()) (9 () ())) (5 (6 () ()) (3 () (2 () (1 () ())))))

;problem 5
(define (great-equal T n)
  (define (count-node T)
    (if (null? T)
        0
        (+ 1 (count-node (left T)) (count-node (right T)))))
  (cond ((null? T) 0)
        ((= (value T) n) (+ 1 (count-node (right T))))
        ((> (value T) n) (+ 1 (great-equal (left T) n) (count-node (right T))))
        ((< (value T) n) (great-equal (right T) n))))

;test case
(great-equal T2 5)
;When I run the test case I got:
;6





