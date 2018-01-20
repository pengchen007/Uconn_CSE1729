;;Peng Chen
;;Peoblem set 10
;;4/24/2017

"problem 1"
(define (make-internal-node 0-tree 1-tree)
  (list 'internal 0-tree 1-tree))

(define (make-H-tree lst)
  (define (getmin l cur-min)
    (cond ((null? l) cur-min)
          ((< (cdr (car l)) (cdr cur-min)) (getmin (cdr l) (car l)))
          (else (getmin (cdr l) cur-min))))
  (define (remove-min l)
    (cond ((null? l) '())
          ((eq? (car l) (getmin l (car l))) (cdr l))
          (else (cons (car l) (remove-min (cdr l))))))
  (define (helper l cur-tree)
    (cond ((null? l) cur-tree)
          ((null? cur-tree) (helper (remove-min l) (getmin l (car l))))
          (else (helper (remove-min l) (make-internal-node cur-tree (getmin l (car l)))))))
  (helper lst '()))
;test cases
(make-H-tree '((#\a . 2013) (#\b . 507) (#\c . 711)))
;When I run the case I got
;'(internal (internal (#\b . 507) (#\c . 711)) (#\a . 2013))

"problem 2"
(define (huff-01 huff L)
    (cond ((null? L)
           (append (huff-01 (cadr huff) '(#\1))
                   (huff-01 (caddr huff) '(#\0))))
          ((eq? (car huff) 'internal)
           (append (huff-01 (cadr huff) (append L (cons #\1 '())))
                   (huff-01 (caddr huff) (append L (cons #\0 '())))))
          (else (list (cons (car huff) (list->string L))))))
;test case
(huff-01 (make-H-tree '((#\a . 2013) (#\b . 507) (#\c . 711))) '())
;When I run the case I got
;((#\b . "11") (#\c . "10") (#\a . "0"))

"problem 3"
(define (encode s l)
  (define (trace v l)
    (if (eq? v (car (car l))) (string->list (cdr (car l))) (trace v (cdr l))))
  (define (helper L1 L2)
    (if (null? L1)
        '()
        (append (trace (car L1) L2) (helper (cdr L1) L2))))
  (list->string (helper (string->list s) (huff-01 (make-H-tree l) '()))))
;test cases
(encode "Luck" '((#\L . 3) (#\u . 2) (#\c . 1) (#\k . 2)))
;When I run the case I got
;"011011110"

"problem 4"
(define (decode bits tree)
  (define (choose bit tree)  
    (cond ((eq? bit #\0) (cadr tree))  
          ((eq? bit #\1) (caddr tree))  
          (else 'error)))
  (define (helper bits cur-tree)  
   (if (null? bits)  
      '()  
       (let ((next (choose (car bits) cur-tree)))  
       (if (number? (cdr next))  
           (cons (car next)  
                 (helper (cdr bits) tree))  
           (helper (cdr bits) next)))))  
 (list->string (helper (string->list bits) tree)))    
;test cases
(define mytree (make-H-tree '((#\L . 3) (#\u . 2) (#\c . 1) (#\k . 2))))
mytree
(decode "100100001" mytree)
;When I run the case I got
;(internal (internal (internal (#\c . 1) (#\u . 2)) (#\k . 2)) (#\L . 3))
;"Luck"

"problem 5"
(define (h-code-object l)
  (let ((tree (make-H-tree l)))
    (define (e-code str)
     (encode str l))
    (define (d-code bits)
      (decode bits tree))
    (define (dispatch method)
      (cond ((eq? method 'encode) e-code)
            ((eq? method 'decode) d-code)))
    dispatch))
;test cases
(define stars (h-code-object '((#\M . 6)(#\a . 5) (#\i . 3) (#\l . 1))))
((stars 'encode) "Mail")
((stars 'decode) "101001000")
;When I run the case I got
;"101001000"
;"Mail"