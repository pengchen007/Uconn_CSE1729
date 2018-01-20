;Peng Chen
;Lab 11
;4/11/2017

"problem 1"
(define (new-account initial-balance)
  (let ((balance initial-balance) (rate 0.01))
    (define (deposit f)
      (begin
        (set! balance
              (+ balance f))
        balance))
    (define (withdraw f)
          (begin
           (set! balance
                 (- balance f))
           balance))
    (define (accrue)  
        (if (< balance 0)
            balance
            (begin
              (set! balance
                    (+ balance (* balance rate)))
              balance)))
    (define (setrate newrate)
      (begin
        (set! rate newrate)
      rate))
    (define (bal-inq) balance)
    (lambda (method)
      (cond ((eq? method 'deposit) deposit)
            ((eq? method 'withdraw) withdraw)
            ((eq? method 'balance-inquire) bal-inq)
            ((eq? method 'accrue) accrue)
            ((eq? method 'setrate) setrate)))))
;test case
(define my-acct (new-account 200))
((my-acct 'accrue))
((my-acct 'deposit) 500)
((my-acct 'withdraw) 200)
((my-acct 'setrate) 0.1)
((my-acct 'accrue))
;when I run the test case I got:
;202.0
;702.0
;502.0
;0.1
;552.2

"problem 2"
(define my-acct1 (new-account 200))
((my-acct1 'accrue))
((my-acct1 'deposit) 500)
((my-acct1 'withdraw) 300)
((my-acct1 'setrate) 0.1)
((my-acct1 'accrue))
;when I run the test case I got:
;202.0
;702.0
;402.0
;0.1
;442.2
;I creat a account1 and set the initial balance is 200
;add 1 year of simple interest to the balance is 202
;after deposit 500, the balance is 702
;after withdraw 300, the balance is 402
;change the rate 0.1, balance is still 402
;add 1 year of simple interest to the balance is 442.2

(define my-acct2 (new-account 300))
((my-acct2 'accrue))
((my-acct2 'deposit) 500)
((my-acct2 'withdraw) 600)
((my-acct2 'setrate) 0.2)
((my-acct2 'accrue))
;when I run the test case I got:
;303.0
;803.0
;203.0
;0.2
;243.6
;I creat a account2 and set the initial balance is 300
;add 1 year of simple interest to the balance is 303
;after deposit 500, the balance is 803
;after withdraw 300, the balance is 203
;change the rate 0.2, balance is still 203
;add 1 year of simple interest to the balance is 243.6

"problem 3"
;(define (tree-it ta tb)
;  (define (weight T)
;    (if (null? T)
;        0
;        (cdar T)))
;  (< (weight ta) (weight tb)))


(define (make-heap lt?)
  (let ((myHeap '()) (q-size 0))    
    (define (make-tree value left right)
      (list value left right))
    (define (value T) (car T))
    (define (left T) (cadr T))
    (define (right T) (caddr T))
    
    (define (combine H1 H2)
      (cond ((null? H1) H2)
            ((null? H2) H1)
            ((lt? (value H1) (value H2)) (combine H2 H1))
            (else (make-tree (value H1)
                             (combine (left H2) (right H2))
                             H2))))
    (define (heap-insert x H)
      (cond ((null? H) (make-tree x '() '()))
            ((lt? x (value H)) (make-tree x
                                           (heap-insert (value H) (right H))
                                           (left H)))
            (else (make-tree (value H)
                             (heap-insert x (right H))
                             (left H)))))
    (define (empty?) (null? myHeap))
    (define (insert x)
      (begin
        (set! q-size (+ q-size 1))
        (set! myHeap (heap-insert x myHeap))))
    (define (extract-min)
      (let ((ex-small (value myHeap)))
        (begin
          (set! q-size (- q-size 1))
          (set! myHeap (combine (left myHeap)
                                 (right myHeap)))
              ex-small)))
    (define (size) q-size)
    (define (show) myHeap)
    
    (lambda (method)
      (cond ((eq? method 'empty) empty?)
            ((eq? method 'insert) insert)
            ((eq? method 'size) size)
            ((eq? method 'show) show)
            ((eq? method 'extract-min) extract-min)))))

;test case
(define Heap1 (make-heap <))
((Heap1 'empty))
((Heap1 'size))
((Heap1 'insert) 5)
((Heap1 'insert) 6)
((Heap1 'show))
((Heap1 'empty))
((Heap1 'size))
((Heap1 'insert) 4)
((Heap1 'insert) 8)
((Heap1 'insert) 7)
((Heap1 'insert) 3)
((Heap1 'insert) 1)
((Heap1 'show))
((Heap1 'size))
((Heap1 'extract-min))
;when I run the test case I got:
;#t
;0
;(5 (6 () ()) ())
;#f
;2
;(1 (3 (5 () ()) (7 () ())) (4 (6 () ()) (8 () ())))
;7
;1













